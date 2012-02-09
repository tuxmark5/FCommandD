{-# LANGUAGE OverloadedStrings #-}

{-
  TODO:
    match :0 and :0.0
    match by dbus addr?
    session termination
    chdir into HOME on runAs
-}

module F.CommandD.Observer.SessionObserver
( Session(..)
, SessionEvent(..)
, SessionObserver(..)
, exportSessionVar
, getEnvironmentPairs
, getSessionVar
, newSessionObserver
) where
  
{- ########################################################################################## -}
import            Control.Concurrent (forkIO)
import            Control.Monad (forever, forM_, when)
import            Control.Monad.Trans.Reader 
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as B
import            Data.IORef
import            Data.List (find, lookup)
import            Data.Maybe (fromMaybe)
import            Data.Map (Map)
import qualified  Data.Map as M
import            F.CommandD.Chan
import            F.CommandD.Daemon
import            F.CommandD.Observer.ProcessObserver
import            System.IO
import            System.Posix.Env (setEnv)
import            System.Posix.IO (closeFd)
{- ########################################################################################## -}

data Session = Session
  { sesAddress          :: ByteString
  , sesDisplay          :: ByteString
  , sesEnvironment      :: Environment
  , sesHome             :: ByteString
  , sesPID              :: Int
  , sesXAuthority       :: ByteString
  }
  
data SessionEvent
  = SessionCreated    Session
  | SessionDestroyed  Session

data SessionObserver = SessionObserver
  { soChan              :: ChanO SessionEvent
  , soProcs             :: [ByteString]
  , soProcChan          :: ChanI ProcessEvent
  , soSessions          :: IORef (Map Int Session)
  }

{- ########################################################################################## -}

type SesM a = ReaderT SessionObserver IO a

addSession :: Session -> SesM ()
addSession ses = ask >>= \s -> lift $ do
  modifyIORef (soSessions s) $ M.insert (sesPID ses) ses
  writeChan (soChan s) (SessionCreated ses)
  
exportSessionVar :: Session -> ByteString -> IO ()
exportSessionVar ses var = setEnv (B.unpack $ var) (B.unpack $ getSessionVar ses var) True
  
-- TODO: migrate to ByteStrings
getEnvironmentPairs :: Session -> [(String, String)]
getEnvironmentPairs = map (\(a, b) -> (B.unpack a, B.unpack b)) . sesEnvironment
  
getProcSession :: Int -> SesM Session
getProcSession pid = lift $ do
  env <- getProcEnv pid
  return $ foldl folder newSession
    { sesEnvironment  = env
    , sesPID          = pid
    } env where
  folder s ("DBUS_SESSION_BUS_ADDRESS", v)  = s { sesAddress    = v }
  folder s ("DISPLAY",                  v)  = s { sesDisplay    = B.take 2 v }
  folder s ("HOME",                     v)  = s { sesHome       = v }
  folder s ("XAUTHORITY",               v)  = s { sesXAuthority = v }
  folder s _                                = s
    
getSession :: ByteString -> SesM (Maybe Session)
getSession name = asks soSessions >>= \sesVar -> lift $ do
  readIORef sesVar >>= return . find' (\v -> sesDisplay v == name)
  
getSessionVar :: Session -> ByteString -> ByteString
getSessionVar ses var = fromMaybe "" $ lookup var (sesEnvironment ses)

handleProcessCreated :: Int -> SesM ()
handleProcessCreated pid = do
  matchSession pid >>= \m -> when m $ do
    ses1  <- getProcSession pid
    ses0' <- getSession (sesDisplay ses1)
    case ses0' of
      Just _    -> return ()
      Nothing   -> addSession ses1

handleProcessDestroyed :: Int -> SesM ()
handleProcessDestroyed pid = ask >>= \obs -> do
  ses <- removeSession pid
  case ses of
    Just ses'   -> lift $ writeChan (soChan obs) $ SessionDestroyed ses'
    Nothing     -> return ()
      
matchSession :: Int -> SesM Bool
matchSession pid = asks soProcs >>= \procs -> lift $ do
  line <- getProcCmdLine pid
  return $ case line of
    cmd:_     -> any (== cmd) procs
    otherwise -> False
    
modifySessions :: (Map Int Session -> (Map Int Session, a)) -> SesM a
modifySessions mod = asks soSessions >>= \sesVar -> lift $ do
  ses <- readIORef sesVar
  let (ses', a) = mod ses
  writeIORef sesVar ses'
  return a
  
monitorLoop :: SesM ()
monitorLoop = ask >>= \obs -> forever $ do
  event <- lift $ readChan (soProcChan obs)
  case event of
    ProcessCreated    pid -> handleProcessCreated pid
    ProcessDestroyed  pid -> handleProcessDestroyed pid
    
newSession :: Session
newSession = Session
  { sesAddress      = ""
  , sesDisplay      = ""
  , sesHome         = "/"
  , sesEnvironment  = []
  , sesPID          = 0
  , sesXAuthority   = ""
  } 

newSessionObserver :: ChanI ProcessEvent -> [ByteString] -> IO (SessionObserver, ChanI SessionEvent)
newSessionObserver pchan procs = do
  (chanI, chanO)  <- newChan
  sessions        <- newIORef M.empty
  let so = SessionObserver chanO procs pchan sessions
  forkIO $ runReaderT monitorLoop so
  return (so, chanI)
  
removeSession :: Int -> SesM (Maybe Session)
removeSession pid = modifySessions mod where
  mod s = case M.lookup pid s of
    Just ses  -> (M.delete pid s, Just ses)
    Nothing   -> (s, Nothing)

{- ########################################################################################## -}
    
find' :: Ord k => (a -> Bool) -> Map k a -> Maybe a
find' p m = case find (\(_, a) -> p a) (M.toList m) of
  Just (_, a) -> Just a
  Nothing     -> Nothing

{- ########################################################################################## -}

