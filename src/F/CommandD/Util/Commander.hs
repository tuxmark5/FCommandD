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
, setFocusHook
, setSessionFilter
, toVariant       -- dbus
) where
  
{- ########################################################################################## -}  
import            Control.Concurrent (forkIO)
import            Control.Concurrent.MVar
import            Control.Exception (handle)
import            Control.Monad (forever, when)
import            Control.Monad.Trans.State (StateT(..), evalStateT, gets, modify)
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as B
import            Data.IORef
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
  { cmdFocusHook  :: FocusEvent -> IO ()
  , cmdHub        :: Sink HubFilter
  , cmdProcObs    :: ProcessObserver
  , cmdProfId     :: Session -> IO ByteString
  , cmdProfiles   :: MVar [Profile]
  , cmdSesChan    :: ChanI SessionEvent
  , cmdSesFilter  :: ByteString -> Bool
  , cmdSesObs     :: SessionObserver
  }
  
data Profile = Profile
  { prClient      :: Maybe Client
  , prFocus       :: IORef FocusEvent
  , prName        :: ByteString
  , prSession     :: Maybe Session
  , prSink        :: Maybe SinkA
  , prTrigger     :: Profile -> IO ()
  }
  
instance Eq Profile where
  a == b = prName a == prName b

{- ########################################################################################## -}

addSink :: SinkC s => ByteString -> (Sink s) -> CmdM ()
addSink name (Sink sink) = gets cmdProfiles >>= \pv -> lift $ do
  list <- takeMVar pv
  pr   <- newProfile name (Just $ SinkA sink)
  putMVar pv (list ++ [pr])

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

setFocusHook :: (FocusEvent -> IO ()) -> CmdM ()
setFocusHook hook = modify $ \s -> s { cmdFocusHook = hook }

setSessionFilter :: (ByteString -> Bool) -> CmdM ()
setSessionFilter filt = modify $ \s -> s { cmdSesFilter = filt }

withSession :: Commander -> (Session -> CD ()) -> CD ()
withSession cmd m = do
  p <- lift $ getFirstProfile cmd
  case prSession p of
    Just session  -> m session
    Nothing       -> lift $ B.putStrLn $ B.concat ["No session for this profile: ", prName p]
    
{- ########################################################################################## -}

getFirstProfile :: Commander -> IO Profile
getFirstProfile cmd = readMVar (cmdProfiles cmd) >>= return . head

isActiveProfile :: Commander -> Profile -> IO Bool
isActiveProfile cmd p = getFirstProfile cmd >>= return . (== p)

loopFocus :: Commander -> Profile -> ChanI FocusEvent -> IO ()
loopFocus cmd p fc = do
  e <- readChan fc
  case e of
    FocusChanged {} -> onFocusChanged cmd p e >> loopFocus cmd p fc
    FocusDestroyed  -> return ()

loopSession :: Commander -> IO ()
loopSession cmd = forever $ do
  e <- readChan (cmdSesChan cmd)
  case e of
    SessionCreated    ses -> onSessionCreated cmd ses
    SessionDestroyed  ses -> onSessionDestroyed cmd ses

modifyProfile :: Commander -> ByteString -> (Profile -> Profile) -> IO Profile
modifyProfile cmd name mod = modifyMVar (cmdProfiles cmd) $ \pr0 -> do
  let filt p        = prName p == name
      mod' (Just p) = return $ mod p
      mod' Nothing  = newProfile name Nothing
  replaceOne filt mod' pr0

modifyProfiles :: Commander -> ([Profile] -> [Profile]) -> IO ()
modifyProfiles cmd mod = modifyMVar_ (cmdProfiles cmd) $ \p0 -> do
  let p1 = mod p0
  case prSink $ head p1 of
    (Just (SinkA s))  -> hubSetSink (cmdHub cmd) (Sink s)
    otherwise         -> return ()
  return p1

newCommander :: (Session -> IO ByteString) -> CmdM () -> CD (Commander, Sink HubFilter)
newCommander profId m = do    
  hub         <- newHub
  proVar      <- lift $ newMVar []  
  
  let cmd0 = Commander {
      cmdFocusHook  = \_ -> return ()  
    , cmdHub        = hub
    , cmdProcObs    = undefined
    , cmdProfId     = profId
    , cmdProfiles   = proVar
    , cmdSesChan    = undefined
    , cmdSesFilter  = \s -> any (== s) ["dwm", "fmonad", "xfce4-session"] 
    , cmdSesObs     = undefined
    }

  (_, cmd1)   <- lift $ runStateT m cmd0
  inotify     <- gets daeINotify
  (pobs, pc)  <- lift $ newProcessObserver inotify
  (sobs, sc)  <- lift $ newSessionObserver pc $ cmdSesFilter cmd1

  let cmd2 = cmd1 {
      cmdProcObs    = pobs
    , cmdSesChan    = sc
    , cmdSesObs     = sobs
    }
  
  profile0      <- lift $ getFirstProfile cmd2
  lift $ case prSink profile0 of
    (Just (SinkA s))  -> hubSetSink hub (Sink s)
    otherwise         -> return ()
  
  lift $ forkIO $ loopSession cmd2
  return (cmd2, hub)
  
newProfile :: ByteString -> Maybe SinkA -> IO Profile
newProfile name sink = do
  focus <- newIORef newFocusEvent
  return Profile
    { prClient      = Nothing
    , prFocus       = focus
    , prName        = name
    , prSession     = Nothing
    , prSink        = sink
    , prTrigger     = \_ -> return ()
    }
    
onFocusChanged :: Commander -> Profile -> FocusEvent -> IO ()
onFocusChanged cmd pr fe = do
  writeIORef (prFocus pr) fe
  active <- isActiveProfile cmd pr
  when active $ cmdFocusHook cmd fe

onSessionCreated :: Commander -> Session -> IO ()
onSessionCreated cmd ses = do
  putStrLn $ show $ sesDisplay ses  
  -- Connect to DBus
  let (Just ad)  = addresses $ decodeUtf8 $ sesAddress ses
  client        <- connectFirst ad
  
  -- Update profile
  name          <- cmdProfId cmd ses
  profile       <- modifyProfile cmd name $ \p0 -> p0
    { prClient  = client
    , prSession = Just ses    
    }    
    
  -- Connect to X
  startFocusObserver cmd profile
  
onSessionDestroyed :: Commander -> Session -> IO ()
onSessionDestroyed cmd ses = do
  putStrLn $ "[-] Session destroyed: " ++ (show $ sesDisplay ses)
  
startFocusObserver :: Commander -> Profile -> IO ()
startFocusObserver cmd pr = do
  let ses = fromJust $ prSession pr
  setEnv "XAUTHORITY" (B.unpack $ sesXAuthority ses) True
  (_, fc) <- newFocusObserver $ sesDisplay ses
  forkIO $ loopFocus cmd pr fc
  return ()
  
{- ########################################################################################## -}
  
connectFirst :: [Address] -> IO (Maybe Client)
connectFirst addrs = loop addrs where
  loop []     = return Nothing
  loop (a:as) = handle (\(e :: ConnectionError) -> loop as) $ do
    putStrLn $ "[DBUS] Connecting " ++ (show a)
    connect a >>= return . Just

replaceOne :: (a -> Bool) -> (Maybe a -> IO a) -> [a] -> IO ([a], a)
replaceOne _    rep [      ] = rep Nothing >>= \p -> return $ ([p], p)
replaceOne pred rep (a:rest) = case pred a of
  True    -> rep (Just a)             >>= \a'     -> return (a':rest, a')
  False   -> replaceOne pred rep rest >>= \(l, r) -> return ( a:l,    r)

{- ########################################################################################## -}
