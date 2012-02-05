{-# LANGUAGE OverloadedStrings #-}

{-
  TODO:
    match :0 and :0.0
    match by dbus addr?
    session termination
    runFirst :0 or :1 or :2 ...
    chdir into HOME on runAs
-}

module F.CommandD.Monitor.SessionMonitor
( Session(..)
, SessionEvent(..)
, SessionMonitor(..)
, newSessionMonitor
, runAs
, runAsSession
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
import            F.CommandD.Monitor.ProcessMonitor
import            System.Exit (ExitCode(..))
import            System.IO
import            System.Posix
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

data SessionMonitor = SessionMonitor
  { smChan              :: ChanO SessionEvent
  , smProcs             :: [ByteString]
  , smProcChan          :: ChanI ProcessEvent
  , smSessions          :: IORef (Map ByteString Session)
  }

{- ########################################################################################## -}

type SesM a = ReaderT SessionMonitor IO a

addSession :: Session -> SesM ()
addSession ses = ask >>= \s -> lift $ do
  modifyIORef (smSessions s) $ M.insert (sesDisplay ses) ses
  writeChan (smChan s) (SessionCreated ses)

getProcSession :: Int -> SesM Session
getProcSession pid = lift $ do
  env <- getProcEnv pid
  return Session
    { sesAddress      = fromMaybe "" $ lookup "DBUS_SESSION_BUS_ADDRESS" env
    , sesDisplay      = fromMaybe "" $ lookup "DISPLAY" env
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

newSessionMonitor :: ChanI ProcessEvent -> [ByteString] -> IO (SessionMonitor, ChanI SessionEvent)
newSessionMonitor pchan procs = do
  (chanI, chanO)  <- newChan
  sessions        <- newIORef M.empty
  let sm = SessionMonitor chanO procs pchan sessions
  forkIO $ runReaderT monitorLoop sm
  return (sm, chanI)
  
getEnvironmentPairs :: Session -> [(String, String)]
getEnvironmentPairs = map (\(a, b) -> (B.unpack a, B.unpack b)) . sesEnvironment
  
runAs :: SessionMonitor -> ByteString -> FilePath -> [String] -> IO ()
runAs mon sesName cmd args = do
  ses' <- runReaderT (getSession sesName) mon
  case ses' of
    Just ses  -> runAsSession ses 1000 cmd args
    Nothing   -> return ()

runAsSession :: Session -> CUid -> FilePath -> [String] -> IO ()
runAsSession ses uid cmd args = do
  putStrLn $ show $ getEnvironmentPairs ses
  forkProcess $ do
    pid <- forkProcess $ do
      executeFile "/usr/bin/sudo" True (["-E", "-u", "#1000", cmd] ++ args) $ Just $ getEnvironmentPairs ses
    createProcessGroupFor pid
    exitImmediately ExitSuccess
  return () 
    
runAsSession' :: Session -> CUid -> FilePath -> [String] -> IO ()
runAsSession' ses uid cmd args = do
  forkProcess $ do
    createSession
    pid <- forkProcess $ do
      setUserID uid
      executeFile cmd True args $ Just $ getEnvironmentPairs ses
    createProcessGroupFor pid
    exitImmediately ExitSuccess
  return ()
  
{- ########################################################################################## -}

