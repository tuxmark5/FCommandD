module F.CommandD.Action.Run
( run
, run1
, runInShellSession
, runInSession
) where

{- ########################################################################################## -}
import            Control.Monad.Trans.State (gets)
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as B
import            F.CommandD.Core (lift)
import            F.CommandD.Observer.ProcessObserver (isProcessRunning)
import            F.CommandD.Observer.SessionObserver
import            F.CommandD.Util.Commander
import            System.Exit (ExitCode(..))
import            System.Posix
import            System.Posix.Directory (changeWorkingDirectory)
{- ########################################################################################## -}
  
instance CommandC Commander String where 
  runCommand cmd s = forkIO_ $ withProfile cmd $ \pro -> runCommand (cmd, pro) s
instance CommandC (Commander, Profile) String where 
  runCommand pro s = runCommand pro $ run s []

{- ########################################################################################## -}

run :: String -> [String] -> ProM ()
run app args = withSession $ \ses -> let
  run app []    = runInShellSession ses app
  run app args  = runInSession      ses app args
  in lift $ run app args

run1 :: ByteString -> ByteString -> ProM ()
run1 app args = withSession $ \ses -> do
  cmd <- gets fst
  r   <- lift $ isProcessRunning (cmdProcObs cmd) app
  if not r
    then run (B.unpack $ B.concat [app, args]) []
    else return ()

runInSession :: Session -> FilePath -> [String] -> IO ()
runInSession ses cmd args = do
  pid0 <- forkProcess $ do
    createSession
    pid <- forkProcess $ do
      -- setUserID uid
      -- changeWorkingDirectory $ B.unpack $ sesHome ses
      executeFile cmd True args $ Just $ getEnvironmentPairs ses
    createProcessGroupFor pid
    exitImmediately ExitSuccess
  getProcessStatus True True pid0 -- prevent defunct process
  return ()
  
runInShellSession :: Session -> String -> IO ()
runInShellSession ses cmd = runInSession ses "/usr/bin/sudo" args
  where args = ["-E", "-b", "-u", '#':(show $ sesUID ses), "/bin/sh", "-c", cmd]

{- ########################################################################################## -}
