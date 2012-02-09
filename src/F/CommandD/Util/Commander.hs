{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module F.CommandD.Util.Commander
( CmdM
, Commander(..)
, MethodCall(..)  -- dbus
, Profile(..)
, addSink
, getCallCmd
, getRunCmd
, nextProfile
, newCommander
, toVariant       -- dbus
) where
  
{- ########################################################################################## -}  
import            Control.Concurrent (forkIO)
import            Control.Concurrent.MVar
import            Control.Exception (handle)
import            Control.Monad (forever)
import            Control.Monad.Trans.State (StateT(..), evalStateT, gets)
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as B
import            Data.Maybe (fromJust)
import            Data.Text.Encoding (decodeUtf8)
import            DBus.Address
import            DBus.Client
import            DBus.Connection (ConnectionError(..))
import            DBus.Message (Flag(..), MethodCall(..))
import            DBus.Types
import            F.CommandD.Action.Run
import            F.CommandD.Chan
import            F.CommandD.Daemon
import            F.CommandD.Filter.HubFilter
import            F.CommandD.Observer.FocusObserver
import            F.CommandD.Observer.ProcessObserver
import            F.CommandD.Observer.SessionObserver
import            F.CommandD.Sink
import            System.Posix.Env (setEnv)
{- ########################################################################################## -}

type CmdM a = StateT Commander IO a

data Commander = Commander
  { cmdHub        :: Sink HubFilter
  , cmdProcObs    :: ProcessObserver
  , cmdProfId     :: Session -> IO ByteString
  , cmdProfiles   :: MVar [Profile]
  , cmdSesChan    :: ChanI SessionEvent
  , cmdSesFilter  :: ByteString -> Bool
  , cmdSesObs     :: SessionObserver
  }
  
data Profile = Profile
  { prSession     :: Maybe Session
  , prClient      :: Maybe Client
  , prName        :: ByteString
  , prSink        :: Maybe SinkA
  , prTrigger     :: Profile -> IO ()
  }

{- ########################################################################################## -}

addSink :: SinkC s => ByteString -> (Sink s) -> CmdM ()
addSink name (Sink sink) = gets cmdProfiles >>= \pv -> do
  list <- lift $ takeMVar pv
  let pr = (newProfile name) { prSink = Just (SinkA sink) }
  lift $ putMVar pv (list ++ [pr])

getCallCmd :: Commander -> CD (MethodCall -> CD ())
getCallCmd cmd = return $ \c -> lift $ do
  p <- getFirstProfile cmd
  case prClient p of
    Just client   -> call_ client c >> return ()
    Nothing       -> B.putStrLn $ B.concat ["No client for this profile: ", prName p]
  
getRunCmd :: Commander -> CD (String -> [String] -> CD ())
getRunCmd cmd = return $ \app args -> withSession cmd $ \ses -> lift $ let
  run app []    = runInShell    ses 1000 app
  run app args  = runInSession  ses 1000 app args
  in run app args
  
nextProfile :: Commander -> CD ()
nextProfile cmd = lift $ modifyProfiles cmd $ \(a:r) -> r ++ [a]

withSession :: Commander -> (Session -> CD ()) -> CD ()
withSession cmd m = do
  p <- lift $ getFirstProfile cmd
  case prSession p of
    Just session  -> m session
    Nothing       -> lift $ B.putStrLn $ B.concat ["No session for this profile: ", prName p]
    
{- ########################################################################################## -}

getFirstProfile :: Commander -> IO Profile
getFirstProfile cmd = readMVar (cmdProfiles cmd) >>= return . head

loopFocus :: Commander -> ChanI FocusEvent -> IO ()
loopFocus cmd cf = forever $ do
  e <- readChan cf
  putStrLn $ show e

loopSession :: Commander -> IO ()
loopSession cmd = forever $ do
  e <- readChan (cmdSesChan cmd)
  case e of
    SessionCreated    ses -> onSessionCreated cmd ses
    SessionDestroyed  ses -> onSessionDestroyed cmd ses

newCommander :: (Session -> IO ByteString) -> CmdM () -> CD (Commander, Sink HubFilter)
newCommander profId m = do
  inotify     <- gets daeINotify
  (pobs, pc)  <- lift $ newProcessObserver inotify
  (sobs, sc)  <- lift $ newSessionObserver pc ["dwm", "fmonad", "xfce4-session"] 
  hub         <- newHub
  proVar      <- lift $ newMVar []
  
  let cmd = Commander { 
      cmdHub        = hub
    , cmdProcObs    = pobs
    , cmdProfId     = profId
    , cmdProfiles   = proVar
    , cmdSesChan    = sc
    , cmdSesFilter  = \s -> any (== s) ["dwm", "xfce4-session"] 
    , cmdSesObs     = sobs
    }
    
  (_, cmd1)     <- lift $ runStateT m cmd
  profile0      <- lift $ getFirstProfile cmd
  lift $ case prSink profile0 of
    (Just (SinkA s))  -> hubSetSink hub (Sink s)
    otherwise         -> return ()
  
  lift $ forkIO $ loopSession cmd1
  return (cmd1, hub)
  
newProfile :: ByteString -> Profile
newProfile name = Profile
  { prSession   = Nothing
  , prClient    = Nothing
  , prName      = name
  , prSink      = Nothing
  , prTrigger   = \_ -> return ()
  }
  
modifyProfile :: Commander -> ByteString -> (Profile -> Profile) -> IO ()
modifyProfile cmd name mod = modifyMVar_ (cmdProfiles cmd) $ \pr0 -> return $ let
  mod' p = if prName p == name 
    then (Just $ mod p) 
    else Nothing
  in replaceOne mod' (newProfile name) pr0

modifyProfiles :: Commander -> ([Profile] -> [Profile]) -> IO ()
modifyProfiles cmd mod = modifyMVar_ (cmdProfiles cmd) $ \p0 -> do
  let p1 = mod p0
  case prSink $ head p1 of
    (Just (SinkA s))  -> hubSetSink (cmdHub cmd) (Sink s)
    otherwise         -> return ()
  return p1
  
onSessionCreated :: Commander -> Session -> IO ()
onSessionCreated cmd ses = do
  putStrLn $ show $ sesDisplay ses
  -- Start the FocusObserver
  setEnv "XAUTHORITY" (B.unpack $ sesXAuthority ses) True
  (fobs, fc)    <- newFocusObserver $ sesDisplay ses
  forkIO $ loopFocus cmd fc
  
  -- Connect to DBus
  let (Just ad)  = addresses $ decodeUtf8 $ sesAddress ses
  client        <- connectFirst ad
  
  -- Update profile
  name          <- cmdProfId cmd ses
  modifyProfile cmd name $ \p0 -> p0
    { prSession = Just ses
    , prClient  = client
    }
  
onSessionDestroyed :: Commander -> Session -> IO ()
onSessionDestroyed cmd ses = do
  putStrLn $ "[-] Session destroyed: " ++ (show $ sesDisplay ses)
  
{- ########################################################################################## -}
  
connectFirst :: [Address] -> IO (Maybe Client)
connectFirst addrs = loop addrs where
  loop []     = return Nothing
  loop (a:as) = handle (\(e :: ConnectionError) -> loop as) $ do
    putStrLn $ "[DBUS] Connecting " ++ (show a)
    connect a >>= return . Just

replaceOne :: (a -> Maybe a) -> a -> [a] -> [a]
replaceOne _ d [      ] = [d]
replaceOne p d (a:rest) = case p a of
  Just a' -> a':rest
  Nothing ->  a:replaceOne p d rest

{- ########################################################################################## -}
