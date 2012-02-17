{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module F.CommandD.Filter.MacroFilter
( CommandC(..)
, MacroFilter(..)
, Mode(..)
, ModeM
, aliasDev
, aliasKey
, command
, commandN
, newMacroFilter
, mode
, mode0
, runMode
, withMode
, withRootMode
) where

{- ########################################################################################## -}
import            Control.Concurrent (forkIO)
import            Control.Concurrent.MVar
import            Control.Monad (forM_, replicateM_, sequence_, when)
import            Control.Monad.Trans.State (State(..), StateT(..), evalStateT, gets)
import qualified  Control.Monad.Trans.State as S
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as B
import            Data.Int (Int32)
import            Data.IORef
import            Data.List (find, partition)
import            Data.Word (Word16)
import            F.CommandD.Core
import            F.CommandD.Filter
import            F.CommandD.Filter.MacroParser
import            F.CommandD.Sink
import            F.CommandD.Source (SourceId)
import            F.CommandD.Util.KeyMap
import            System.Linux.Input
{- ########################################################################################## -}

type MacroFilter = MVar MacroFilterS
  
data MacroFilterS = MacroFilterS
  { mfsActions      :: [IO ()] 
  , mfsFiltered     :: Bool
  , mfsFilteredKeys :: [Key]
  , mfsKeyMap       :: KeyMap
  , mfsModes        :: [Mode]
  , mfsNodes        :: [Node]
  , mfsPressedKeys  :: [Key]
  , mfsRootMode     :: Mode
  , mfsSupressed    :: [IO ()]
  }

instance SinkC MacroFilter where
  sinkWrite var = get >>= \e -> do
    let et = eventType e
    case () of 
      _ | et == evMSC -> exitContST () 
        | et == evREL -> onRelEvent var -- appendEvent $ synEvent e
        | et == evSYN -> return () 
        | et == evKEY -> onKeyEvent var
        | otherwise   -> onUnknownEvent

{- ########################################################################################## -}

class CommandC s a where
  runCommand :: s -> a -> IO ()

instance CommandC a (IO ()) where
  runCommand _ a = forkIO a >> return ()

data Mode = Mode
  { modeChildren  :: [Mode]
  , modeEnabled   :: IORef Bool
  , modeName      :: ByteString
  , modeRootNode  :: Node
  }
  
type ModeM s a = StateT (ModeS s) IO a

data ModeS s = ModeS
  { msKeyMap      :: KeyMap
  , msMode        :: Mode
  , msState       :: s
  }

addMacro :: Mode -> IO () -> [Key] -> Mode
addMacro m0 act combo = m0 { modeRootNode = addMacro' (modeRootNode m0) act combo }

addMacro' :: Node -> IO () -> [Key] -> Node
addMacro' n0 act []           = n0 { nodeAction   = Just act }
addMacro' n0 act (first:rest) = n0 { nodeChildren = alter f test (nodeChildren n0) }
  where f (Just n)    = addMacro' n  act rest
        f (Nothing)   = addMacro' n' act rest
        n'            = Node Nothing [] first
        test n        = nodeKey n == first

addMode :: Mode -> Mode -> Mode
addMode par cld = par { modeChildren = cld:modeChildren par }

collectModes :: Mode -> IO [Mode]
collectModes mode = do
  enabled <- readIORef $ modeEnabled mode
  if enabled
    then mapM collectModes (modeChildren mode) >>= return . (mode:) . concat
    else return []

resetModes' :: MacroFilterS -> IO MacroFilterS
resetModes' s = do
  modes <- collectModes (mfsRootMode s)
  return s 
    { mfsModes = modes
    , mfsNodes = map modeRootNode modes
    }

findMode :: Mode -> [ByteString] -> Maybe Mode 
findMode m []       = Just m
findMode m (a:rest) = case findModeChild m a of
  Just mode -> findMode mode rest
  Nothing   -> Nothing

findModeChild :: Mode -> ByteString -> Maybe Mode
findModeChild mode name = find (\m -> modeName m == name) (modeChildren mode)

newMode :: ByteString -> Bool -> IO Mode
newMode name enabled = do
  enabled <- newIORef enabled 
  return $ Mode
    { modeChildren  = []
    , modeEnabled   = enabled
    , modeName      = name
    , modeRootNode  = defaultNode
    }

mode :: ByteString -> ModeM s a -> ModeM s a
mode name m = mode' name True m

mode0 :: ByteString -> ModeM s a -> ModeM s a
mode0 name m = mode' name False m

mode' :: ByteString -> Bool -> ModeM s a -> ModeM s a
mode' name e m = StateT $ \ms -> do
  newMode0  <- newMode name e
  (a, ms1)  <- runStateT m ms { msMode = newMode0 }
  return (a, ms { msMode = addMode (msMode ms) (msMode ms1) }) 

runMode :: Filter MacroFilter -> s -> ModeM s a -> CD a
runMode (Sink f) s m = lift $ modifyMVar f $ \dat0 -> do
  (a, ms1)  <- runStateT m $ ModeS (mfsKeyMap dat0) (mfsRootMode dat0) s
  modes     <- collectModes $ msMode ms1 
  return (dat0 
    { mfsKeyMap     = msKeyMap ms1
    , mfsModes      = modes
    , mfsNodes      = map modeRootNode modes
    , mfsRootMode   = msMode ms1 
    }, a)

printMode :: Int -> Mode -> IO ()
printMode i mode = do
  replicateM_ i $ putStr "  "
  putStrLn $ "<" ++ (B.unpack $ modeName mode) ++ ">"
  forM_ (modeChildren mode) $ printMode (i + 1)
  printNode (i + 1) (modeRootNode mode)

withMode :: Mode -> [ByteString] -> (Mode -> IO ()) -> IO ()
withMode root path m = case findMode root path of
  Just mode -> m mode 
  Nothing   -> return ()

withRootMode :: MacroFilter -> (Mode -> IO ()) -> IO ()
withRootMode mf m = modifyMVar_ mf $ \s -> do 
  m $ mfsRootMode s
  resetModes' s

{- ########################################################################################## -}

data Node = Node
  { nodeAction    :: Maybe (IO ())
  , nodeChildren  :: [Node]
  , nodeKey       :: Key
  }

appendNode :: Node -> MacroM ()
appendNode node = S.modify $ \s -> s { mfsNodes = node:(mfsNodes s) }

defaultNode :: Node
defaultNode = Node
  { nodeAction    = Nothing
  , nodeChildren  = []
  , nodeKey       = defaultKey
  }
  
isLeafNode :: Node -> Bool
isLeafNode node = null $ nodeChildren node
  
modifyNodes :: ([Node] -> ([Node], a)) -> MacroM a
modifyNodes mod =  S.get >>= \s0 -> do
  let (n1, r) = mod $ mfsNodes s0
  S.put s0 { mfsNodes = n1 }
  return r

printNode :: Int -> Node -> IO ()
printNode i node = do
  replicateM_ i $ putStr "  "
  putStrLn $ "[" ++ (show $ nodeKey node) ++ "]"
  forM_ (nodeChildren node) $ printNode (i + 1)

testNode :: Int -> Node -> MacroM ()
testNode 0 node = do
  case nodeAction node of
    Just act  -> appendAction act 
    Nothing   -> return ()
  when ((not $ isLeafNode node) && (not $ isModifier $ nodeKey node)) $ do
    appendNode node
  
testNode depth node = 
  forM_ (nodeChildren node) $ \n -> do
    p <- isKeyPressed (nodeKey n)
    when p $ do
      when (isFiltered $ nodeKey n) filterKey
      testNode (depth - 1) n

testNodes :: MacroM ()
testNodes = removeNodes $ \nodes -> do
  depth <- pressedKeyCount
  forM_ nodes $ testNode depth

removeNodes :: ([Node] -> MacroM a) -> MacroM a
removeNodes fun = S.get >>= \s0 -> do
  S.put s0 { mfsFiltered = False, mfsNodes = [] }
  fun $ mfsNodes s0

resetNodes :: MacroM Bool
resetNodes = do
  s0 <- S.get
  let n1 = mod (mfsNodes s0)
      mod [] = (map modeRootNode $ mfsModes s0)
      mod n  = (n)
  S.put s0 { mfsNodes = n1 }
  return $ mfsFiltered s0

{- ########################################################################################## -}
  
type MacroM a = StateT MacroFilterS IO a

appendAction :: IO () -> MacroM ()
appendAction action = S.modify $ \s -> s { mfsActions = action:(mfsActions s) }

appendEvent :: Event -> CE ()
appendEvent e1 = ContST $ \e0 c -> c e0 () >> c e1 ()

filterKey :: MacroM ()
filterKey = S.modify $ \s -> s { mfsFiltered = True }

isKeyFiltered :: Key -> MacroM Bool
isKeyFiltered key = S.gets mfsFilteredKeys >>= return . any (== key) 

isKeyPressed :: Key -> MacroM Bool
isKeyPressed key = S.gets mfsPressedKeys >>= return . any (== key) 

modifyFilteredKeys :: ([Key] -> [Key]) -> MacroM ()
modifyFilteredKeys mod = S.modify $ \s -> s { mfsFilteredKeys = mod $ mfsFilteredKeys s }

modifyPressedKeys :: ([Key] -> [Key]) -> MacroM ()
modifyPressedKeys mod = S.modify $ \s -> s { mfsPressedKeys = mod $ mfsPressedKeys s }
   
onKeyEvent :: MacroFilter -> CE ()
onKeyEvent = runFilter processKeyEvent 

onRelEvent :: MacroFilter -> CE ()
onRelEvent var = get >>= \e -> do
  when (eventCode e == relWheel) $ runFilter processRelEvent var

onUnknownEvent :: CE ()
onUnknownEvent = get >>= \e -> do
  lift $ putStrLn $ "Unknown event: " ++ (show e)
  return ()

pressedKeyCount :: MacroM Int
pressedKeyCount = S.gets mfsPressedKeys >>= return . length

processKeyEvent :: Event -> MacroM Bool
processKeyEvent e = do
  let key = toKey e
  updatePressedKeys e key
  case eventValue e of
    0 -> removeFilteredKey key
    1 -> do
      testNodes
      f <- resetNodes
      when f $ modifyFilteredKeys (key :)
      return f
    2 -> isKeyFiltered key

processRelEvent :: Event -> MacroM Bool
processRelEvent e = do
  let key = toKey e
  modifyPressedKeys $ \   keys  -> key:keys
  testNodes
  modifyPressedKeys $ \(k:keys) ->     keys
  resetNodes

removeFilteredKey :: Key -> MacroM Bool
removeFilteredKey k = do
  st0 <- S.get
  let (r, fkeys1) = partition (== k) (mfsFilteredKeys st0)
  S.put st0 { mfsFilteredKeys = fkeys1 }
  return $ not $ null r

runFilter :: (Event -> MacroM Bool) -> MacroFilter -> CE ()
runFilter filter var = get >>= \e -> do
  filt <- runMacroM var (filter e) 
  if filt
    then exitContST ()
    else appendEvent $ synEvent e

runMacroM :: MacroFilter -> MacroM a -> CE a
runMacroM var m = lift $ modifyMVar var $ \state0 -> do
  --let (r, state1)  = S.runState m state0
  (r, state1)     <- S.runStateT m state0
  sequence_ (mfsActions state1)
  return (state1 { mfsActions = [] }, r)
  
supressEvent :: MacroFilter -> CE ()
supressEvent var = saveContST $ \s -> do
  lift $ modifyMVar_ var $ \d -> return $ d { mfsSupressed = s:(mfsSupressed d) }
  exitContST ()
  
synEvent :: Event -> Event
synEvent base   = base
  { eventType   = evSYN
  , eventCode   = 0
  , eventValue  = 0
  }
  
toCode :: Event -> Word16
toCode e | eventType e == evKEY = eventCode e 
         | eventType e == evREL = 0x8000 + (eventCode e) * 2 + if eventValue e > 0 then 1 else 0

toKey :: Event -> Key
toKey e = defaultKey 
  { keyCode   = toCode e
  , keyDevice = eventSource e
  }

updatePressedKeys :: Event -> Key -> MacroM ()
updatePressedKeys e key = modifyPressedKeys $ case (eventValue e) of
  0 -> filter (/= key)
  1 -> (key :)
  2 -> id
  
{- ########################################################################################## -}
 
aliasDev :: SourceId -> ByteString -> ModeM s ()
aliasDev sid name = S.modify $ \s -> s { msKeyMap = addDev (msKeyMap s) name sid }
 
aliasKey :: ByteString -> ByteString -> ByteString -> ModeM s ()
aliasKey key0 dev1 key1 = S.get >>= \ms0 -> do
  let key = lookupKey (msKeyMap ms0) key0
      dev = lookupDev (msKeyMap ms0) dev1
  case (dev, key) of
    (Just d,  Just k) -> S.put $ ms0 { msKeyMap = addKey (msKeyMap ms0) key1 d (keyCode k) }
    (d,       k     ) -> lift $ putStrLn $ concat ["[ERROR] Invalid key alias", show d, show k]

command :: CommandC s c => String -> c -> ModeM s ()
command combo cmd = S.get >>= \ms0 -> do 
  case parseMacro (msKeyMap ms0) combo of
    Left  err -> lift $ putStrLn err
    Right seq -> let
      io = runCommand (msState ms0) cmd
      in S.put $ ms0 { msMode = addMacro (msMode ms0) io seq }
  
commandN :: CommandC s c => String -> [c] -> ModeM s ()
commandN combo c = do
  ref     <- lift $ newMVar c
  state   <- gets msState
  command combo $ cycleCommand state ref

cycleCommand :: CommandC s c => s -> MVar [c] -> IO ()
cycleCommand state cmds = do
  c0 <- modifyMVar cmds $ \(a:r) -> return (r ++ [a], a)
  runCommand state c0

newMacroFilter :: CD MacroFilter
newMacroFilter = lift $ do
  mode  <- newMode "" True
  ref   <- newMVar MacroFilterS
    { mfsActions      = []
    , mfsFiltered     = False
    , mfsFilteredKeys = []
    , mfsKeyMap       = defaultKeyMap
    , mfsModes        = []
    , mfsNodes        = []
    , mfsPressedKeys  = []
    , mfsRootMode     = mode
    , mfsSupressed    = []
    }
  return ref
  
{- ########################################################################################## -}

alter :: (Maybe a -> a) -> (a -> Bool) -> [a] -> [a]
alter mod _    []                   = [mod Nothing]
alter mod pred (a:rest) | pred a    = (mod $ Just a):rest
                        | otherwise = a:(alter mod pred rest)

forAnyM :: Monad m => [a] -> (a -> m Bool) -> m Bool
forAnyM list p = mapM p list >>= return . or
  
{- ########################################################################################## -}
