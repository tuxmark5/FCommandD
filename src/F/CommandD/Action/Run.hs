module F.CommandD.Action.Run
( runInSession
, runInShell
) where

{- ########################################################################################## -}
import            F.CommandD.Observer.SessionObserver
import            System.Exit (ExitCode(..))
import            System.Posix
{- ########################################################################################## -}
  
{-runAs :: SessionObserver -> ByteString -> FilePath -> IO ()
runAs mon sesName cmd = do
  ses' <- runReaderT (getSession sesName) mon
  case ses' of
    Just ses  -> runAsSession ses 1000 cmd
    Nothing   -> return ()
    -}
    
runInSession :: Session -> CUid -> FilePath -> [String] -> IO ()
runInSession ses uid cmd args = do
  forkProcess $ do
    createSession
    pid <- forkProcess $ do
      -- setUserID uid
      executeFile cmd True args $ Just $ getEnvironmentPairs ses
    createProcessGroupFor pid
    exitImmediately ExitSuccess
  return ()
  
runInShell :: Session -> CUid -> String -> IO ()
runInShell ses uid cmd = runInSession ses 1000 "/usr/bin/sudo" args
  where args = ["-E", "-b", "-u", "#1000", "/bin/sh", "-c", cmd]

{- ########################################################################################## -}
