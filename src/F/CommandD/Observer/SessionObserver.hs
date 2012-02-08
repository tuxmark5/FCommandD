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
, getEnvironmentPairs
, newSessionObserver
) where
  
{- ########################################################################################## -}
import            Control.Concurrent (forkIO)
import            Control.Monad (forever, forM_, when)
import            Control.Monad.Trans.Reader 
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as B
import            Data.IORef
import            Data.List (lookup)
import            Data.Maybe (fromMaybe)
import            Data.Map (Map)
import qualified  Data.Map as M
import            F.CommandD.Chan
import            F.CommandD.Daemon
import            F.CommandD.Observer.ProcessObserver
import            System.IO
import            System.Posix.IO (closeFd)
{- ########################################################################################## -}

data Session = Session
  { sesAddress          :: ByteString
  , sesDisplay          :: ByteString
  , sesEnvironment      :: Environment
  , sesPID              :: Int
  }
  
data SessionEvent
  = SessionCreated    Session
  | SessionDestroyed  Session

data SessionObserver = SessionObserver
  { smChan              :: ChanO SessionEvent
  , smProcs             :: [ByteString]
  , smProcChan          :: ChanI ProcessEvent
  , smSessions          :: IORef (Map ByteString Session)
  }

{- ########################################################################################## -}

type SesM a = ReaderT SessionObserver IO a

addSession :: Session -> SesM ()
addSession ses = ask >>= \s -> lift $ do
  modifyIORef (smSessions s) $ M.insert (sesDisplay ses) ses
  writeChan (smChan s) (SessionCreated ses)

-- TODO: migrate to ByteStrings
getEnvironmentPairs :: Session -> [(String, String)]
getEnvironmentPairs = map (\(a, b) -> (B.unpack a, B.unpack b)) . sesEnvironment
  
getProcSession :: Int -> SesM Session
getProcSession pid = lift $ do
  env <- getProcEnv pid
  return Session
    { sesAddress      =            fromMaybe "" $ lookup "DBUS_SESSION_BUS_ADDRESS" env
    , sesDisplay      = B.take 2 $ fromMaybe "" $ lookup "DISPLAY" env
    , sesEnvironment  = env
    , sesPID          = pid
    }
    
getSession :: ByteString -> SesM (Maybe Session)
getSession name = asks smSessions >>= \sesVar -> lift $ do
  readIORef sesVar >>= return . M.lookup name
  
matchSession :: Int -> SesM Bool
matchSession pid = asks smProcs >>= \procs -> lift $ do
  line <- getProcCmdLine pid
  return $ case line of
    cmd:_     -> any (== cmd) procs
    otherwise -> False
  
monitorLoop :: SesM ()
monitorLoop = ask >>= \mon -> forever $ do
  event <- lift $ readChan (smProcChan mon)
  case event of
    ProcessCreated    pid -> do
      matchSession pid >>= \m -> when m $ do
        ses1  <- getProcSession pid
        ses0' <- getSession (sesDisplay ses1)
        case ses0' of
          Just _    -> return ()
          Nothing   -> addSession ses1
    ProcessDestroyed  pid -> return ()

newSessionObserver :: ChanI ProcessEvent -> [ByteString] -> IO (SessionObserver, ChanI SessionEvent)
newSessionObserver pchan procs = do
  (chanI, chanO)  <- newChan
  sessions        <- newIORef M.empty
  let sm = SessionObserver chanO procs pchan sessions
  forkIO $ runReaderT monitorLoop sm
  return (sm, chanI)
  
{- ########################################################################################## -}

