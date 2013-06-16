module F.CommandD.Util.Commander
( CmdM
, CommandC(..)
, Commander(..)
, Profile(..)
, ProM(..)
, Session(..)
, activate
, addSink
, at
, forkIO_
, getFirstProfile
, initCommander
, nextSession
, newCommander
, setFocusHook
, setSessionFilter
, setSessionSwitchHook
, withProfile
, withSession
) where
  
{- ########################################################################################## -}
import            Control.Concurrent (forkIO)
import            Control.Concurrent.MVar
import qualified  Control.Exception as E
import            Control.Monad (forever, when)
import            Control.Monad.Trans.State (StateT(..), evalStateT, get, gets, modify)
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as B
import            Data.IORef
import            Data.List (find)
import            Data.Maybe (fromJust)
import            Data.Text.Encoding (decodeUtf8)
import            DBus
import            DBus.Client
import            F.CommandD.Core hiding (get)
import            F.CommandD.Filter.HubFilter
import            F.CommandD.Filter.MacroFilter
import            F.CommandD.Observer.FocusObserver
import            F.CommandD.Observer.ProcessObserver
import            F.CommandD.Observer.SessionObserver
import            F.CommandD.Sink
import            F.CommandD.Util.Chan
import            F.CommandD.Util.WindowMatcher
import            System.Posix.Env (setEnv)
{- ########################################################################################## -}

instance CommandC Commander (CmdM ()) where 
  runCommand cmd m = forkIO_ $ evalStateT m cmd
--instance CommandC Commander (IO ()) where 
--  runCommand _   m = forkIO_ $ m
instance CommandC Commander (ProM ()) where 
  runCommand cmd m = forkIO_ $ withProfile cmd $ \pro -> evalStateT m (cmd, pro)

instance CommandC (Commander, Profile) (IO ()) where 
  runCommand _   m = m
instance CommandC (Commander, Profile) (ProM ()) where 
  runCommand pro m = evalStateT m pro

{- ########################################################################################## -}

type CmdM a = StateT Commander IO a

data Commander = Commander
  { cmdFocusHook  :: FocusEvent -> CmdM ()
  , cmdHub        :: HubFilter
  , cmdMacroFilt  :: MacroFilter
  , cmdProcObs    :: ProcessObserver
  , cmdProfId     :: Session -> IO ByteString
  , cmdProfiles   :: MVar [Profile]
  , cmdSesChan    :: ChanI SessionEvent
  , cmdSesFilter  :: ByteString -> Bool
  , cmdSesObs     :: SessionObserver
  , cmdSesHook    :: ProM ()
  }
  
data Profile = Profile
  { prClient      :: Maybe Client
  , prFocus       :: IORef FocusEvent
  , prName        :: ByteString
  , prSession     :: Maybe Session
  , prSink        :: Maybe SinkA
  , prTrigger     :: ProM ()
  }
  
instance Eq Profile where
  a == b = prName a == prName b

{- ########################################################################################## -}

activateProfile :: Commander -> Profile -> IO ()
activateProfile cmd pro = modifyProfiles cmd $ \list -> let
  (a, b) = span (/= pro) list in (b ++ a)

addSink :: SinkC s => ByteString -> (Sink s) -> ProM () -> CmdM ()
addSink name (Sink sink) hook = gets cmdProfiles >>= \pv -> lift $ do
  list <- takeMVar pv
  pr   <- newProfile name (Just $ SinkA sink)
  putMVar pv (list ++ [pr])

setFocusHook :: (FocusEvent -> CmdM ()) -> CmdM ()
setFocusHook hook = modify $ \s -> s { cmdFocusHook = hook }

setSessionFilter :: (ByteString -> Bool) -> CmdM ()
setSessionFilter filt = modify $ \s -> s { cmdSesFilter = filt }

setSessionSwitchHook :: ProM () -> CmdM ()
setSessionSwitchHook hook = modify $ \s -> s { cmdSesHook = hook }

{- ########################################################################################## -}

type ProM a = StateT (Commander, Profile) IO a

activate :: ProM ()
activate = get >>= \(cmd, pro) -> lift $ activateProfile cmd pro

at :: CommandC (Commander, Profile) a => ByteString -> a -> CmdM ()
at name a = get >>= \cmd -> lift $ withProfile' cmd name $ \pro -> runCommand (cmd, pro) a

nextSession :: CmdM ()
nextSession = get >>= \cmd -> lift $ do
  E.catch (modifyProfiles cmd $ \(a:r) -> r ++ [a]) $ \(E.SomeException e) -> do
    putStrLn $ show e

{- ########################################################################################## -}

withSession :: (Session -> ProM ()) -> ProM ()
withSession m = gets snd >>= \p -> do
  case prSession p of
    Just session  -> m session
    Nothing       -> lift $ B.putStrLn $ B.concat ["No session for this profile: ", prName p]
 
withProfile :: Commander -> (Profile -> IO a) -> IO a
withProfile cmd m = getFirstProfile cmd >>= m

withProfile' :: Commander -> ByteString -> (Profile -> IO ()) -> IO ()
withProfile' cmd name m = getProfile cmd name >>= \pr -> case pr of
  Just p    -> m p
  Nothing   -> B.putStrLn $ B.concat ["No profile named with: ", name]
    
{- ########################################################################################## -}

emitFocusEvent :: Commander -> Profile -> IO ()
emitFocusEvent cmd pro = do
  e <- readIORef (prFocus pro)
  evalStateT (cmdFocusHook cmd e) cmd
  return ()

getFirstProfile :: Commander -> IO Profile
getFirstProfile cmd = readMVar (cmdProfiles cmd) >>= return . head

getProfile :: Commander -> ByteString -> IO (Maybe Profile)
getProfile cmd name = readMVar (cmdProfiles cmd) >>= return . find filt where
  filt p = prName p == name

isActiveProfile :: Commander -> Profile -> IO Bool
isActiveProfile cmd p = getFirstProfile cmd >>= return . (== p)

modifyProfile :: Commander -> ByteString -> (Profile -> Profile) -> IO Profile
modifyProfile cmd name mod = modifyMVar (cmdProfiles cmd) $ \pr0 -> do
  let filt p        = prName p == name
      mod' (Just p) = return $ mod p
      mod' Nothing  = newProfile name Nothing
  replaceOne filt mod' pr0

modifyProfiles :: Commander -> ([Profile] -> [Profile]) -> IO ()
modifyProfiles cmd mod = modifyMVar_ (cmdProfiles cmd) $ \p0 -> do
  let p1@(pro:_) = mod p0
  case prSink pro of
    (Just (SinkA s))  -> hubSetSink (cmdHub cmd) s
    otherwise         -> return ()
  forkIO $ do
    emitFocusEvent cmd pro
    evalStateT (cmdSesHook cmd >> prTrigger pro) (cmd, pro)
  return p1

newProfile :: ByteString -> Maybe SinkA -> IO Profile
newProfile name sink = do
  focus <- newIORef newFocusEvent
  return Profile
    { prClient      = Nothing
    , prFocus       = focus
    , prName        = name
    , prSession     = Nothing
    , prSink        = sink
    , prTrigger     = return ()
    }

{- ########################################################################################## -}

initCommander :: Commander -> CD ()
initCommander cmd = lift $ modifyProfiles cmd id 

eql0 :: ByteString -> Bool
eql0 s = any (== s) 
  [ "dwm"
  , "i3"
  , "/usr/bin/i3"
  , "fmonad"
  , "/home/angel/.cabal/bin/fmonad"
  , "lxpanel"
  ]

eql1 :: ByteString -> Bool
eql1 s = any eql0 $ B.split '/' s

newCommander  :: (Session -> IO ByteString) -> CmdM () 
              -> CD (Commander, Sink MacroFilter, Sink HubFilter)
newCommander profId m = do
  hub         <- newHub
  macro       <- newMacroFilter
  proVar      <- lift $ newMVar []  
  
  let cmd0 = Commander {
      cmdFocusHook  = \_ -> return ()  
    , cmdHub        = hub
    , cmdMacroFilt  = macro
    , cmdProcObs    = undefined
    , cmdProfId     = profId
    , cmdProfiles   = proVar
    , cmdSesChan    = undefined
    , cmdSesFilter  = eql0
    , cmdSesObs     = undefined
    , cmdSesHook    = return ()
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
    (Just (SinkA s))  -> hubSetSink hub s
    otherwise         -> return ()
  
  lift $ forkIO $ loopSession cmd2
  return (cmd2, Sink macro, Sink hub)

{- ########################################################################################## -}
 
loopFocus :: Commander -> Profile -> ChanI FocusEvent -> IO ()
loopFocus cmd p fc = do
  e <- readChan fc
  --putStrLn $ show e
  case e of
    FocusChanged {} -> onFocusChanged cmd p e >> loopFocus cmd p fc
    FocusDestroyed  -> return ()

loopSession :: Commander -> IO ()
loopSession cmd = forever $ do
  e <- readChan (cmdSesChan cmd)
  case e of
    SessionCreated    ses -> onSessionCreated cmd ses
    SessionDestroyed  ses -> onSessionDestroyed cmd ses 
  
onFocusChanged :: Commander -> Profile -> FocusEvent -> IO ()
onFocusChanged cmd pr fe = do
  writeIORef (prFocus pr) fe
  active <- isActiveProfile cmd pr
  when active $ evalStateT (cmdFocusHook cmd fe) cmd

onSessionCreated :: Commander -> Session -> IO ()
onSessionCreated cmd ses = do
  putStrLn "Starting session creation"

  putStrLn $ show $ sesDisplay ses  
  -- Connect to DBus
  let (Just ad)  = parseAddresses $ B.unpack $ sesAddress ses -- decodeUtf8
  putStrLn "Connected to DBUS0"
  client        <- connectFirst ad
  putStrLn "Connected to DBUS"
  
  -- Update profile
  name          <- cmdProfId cmd ses
  profile       <- modifyProfile cmd name $ \p0 -> p0
    { prClient  = client
    , prSession = Just ses    
    }    
  putStrLn "Updated profile"
    
  -- Connect to X
  startFocusObserver cmd profile
  putStrLn "Started focus observer"
  
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
  loop (a:as) = E.handle (\(e :: ClientError) -> loop as) $ do
    putStrLn $ "[DBUS] Connecting " ++ (show a)
    connect a >>= return . Just

forkIO_ :: IO () -> IO ()
forkIO_ m = forkIO m >> return ()

replaceOne :: (a -> Bool) -> (Maybe a -> IO a) -> [a] -> IO ([a], a)
replaceOne _    rep [      ] = rep Nothing >>= \p -> return $ ([p], p)
replaceOne pred rep (a:rest) = case pred a of
  True    -> rep (Just a)             >>= \a'     -> return (a':rest, a')
  False   -> replaceOne pred rep rest >>= \(l, r) -> return ( a:l,    r)

{- ########################################################################################## -}
