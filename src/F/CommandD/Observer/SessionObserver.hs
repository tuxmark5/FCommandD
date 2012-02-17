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
import            F.CommandD.Core
import            F.CommandD.Observer.ProcessObserver
import            F.CommandD.Util.Chan
import            System.IO
import            System.Posix (CUid)
import            System.Posix.Env (setEnv)
import            System.Posix.IO (closeFd)
{- ########################################################################################## -}

data Session = Session
  { sesAddress          :: ByteString
  , sesDisplay          :: ByteString
  , sesEnvironment      :: Environment
  , sesHome             :: ByteString
  , sesProc             :: Process
  , sesUID              :: CUid
  , sesXAuthority       :: ByteString
  }
  
data SessionEvent
  = SessionCreated    Session
  | SessionDestroyed  Session

data SessionObserver = SessionObserver
  { soChan              :: ChanO SessionEvent
  , soProcChan          :: ChanI ProcessEvent
  , soSessionFilter     :: ByteString -> Bool
  , soSessions          :: IORef (Map Int Session)
  }

{- ########################################################################################## -}

type SesM a = ReaderT SessionObserver IO a

addSession :: Session -> SesM ()
addSession ses = ask >>= \s -> lift $ do
  modifyIORef (soSessions s) $ M.insert (procPid $ sesProc $ ses) ses
  writeChan (soChan s) (SessionCreated ses)
  
exportSessionVar :: Session -> ByteString -> IO ()
exportSessionVar ses var = setEnv (B.unpack $ var) (B.unpack $ getSessionVar ses var) True
    
getSession :: ByteString -> SesM (Maybe Session)
getSession name = asks soSessions >>= \sesVar -> lift $ do
  readIORef sesVar >>= return . find' (\v -> sesDisplay v == name)

matchSession :: Process -> SesM Bool
matchSession p = ask >>= \s -> lift $ do
  return $ case procCmd p of
    cmd:_     -> soSessionFilter s cmd
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
    ProcessCreated    p -> onProcessCreated p
    ProcessDestroyed  p -> onProcessDestroyed p
    
newSession :: Session
newSession = Session
  { sesAddress      = ""
  , sesDisplay      = ""
  , sesHome         = "/"
  , sesEnvironment  = []
  , sesProc         = Process 0 []
  , sesUID          = 0
  , sesXAuthority   = ""
  } 

newSessionObserver :: ChanI ProcessEvent -> (ByteString -> Bool) -> IO (SessionObserver, ChanI SessionEvent)
newSessionObserver pchan sfilt = do
  (chanI, chanO)  <- newChan
  sessions        <- newIORef M.empty
  let so = SessionObserver chanO pchan sfilt sessions
  forkIO $ runReaderT monitorLoop so
  return (so, chanI)

onProcessCreated :: Process -> SesM ()
onProcessCreated p = do
  matchSession p >>= \m -> when m $ do
    ses1  <- getProcSession p
    ses0' <- getSession (sesDisplay ses1)
    case ses0' of
      Just _    -> return ()
      Nothing   -> addSession ses1

onProcessDestroyed :: Process -> SesM ()
onProcessDestroyed p = ask >>= \obs -> do
  ses <- removeSession $ procPid p
  case ses of
    Just ses'   -> lift $ writeChan (soChan obs) $ SessionDestroyed ses'
    Nothing     -> return ()
      
removeSession :: Int -> SesM (Maybe Session)
removeSession pid = modifySessions mod where
  mod s = case M.lookup pid s of
    Just ses  -> (M.delete pid s, Just ses)
    Nothing   -> (s, Nothing)

{- ########################################################################################## -}

-- TODO: migrate to ByteStrings
getEnvironmentPairs :: Session -> [(String, String)]
getEnvironmentPairs = map (\(a, b) -> (B.unpack a, B.unpack b)) . sesEnvironment

getProcSession :: Process -> SesM Session
getProcSession proc = lift $ do
  env <- getProcEnv $ procPid proc
  uid <- getProcUid $ procPid proc
  return $ foldl folder newSession
    { sesEnvironment  = env
    , sesProc         = proc
    , sesUID          = uid
    } env where
  folder s ("DBUS_SESSION_BUS_ADDRESS", v)  = s { sesAddress    = v }
  folder s ("DISPLAY",                  v)  = s { sesDisplay    = B.take 2 v }
  folder s ("HOME",                     v)  = s { sesHome       = v }
  folder s ("XAUTHORITY",               v)  = s { sesXAuthority = v }
  folder s _                                = s

getSessionVar :: Session -> ByteString -> ByteString
getSessionVar ses var = fromMaybe "" $ lookup var (sesEnvironment ses)

{- ########################################################################################## -}
    
find' :: Ord k => (a -> Bool) -> Map k a -> Maybe a
find' p m = case find (\(_, a) -> p a) (M.toList m) of
  Just (_, a) -> Just a
  Nothing     -> Nothing

{- ########################################################################################## -}

