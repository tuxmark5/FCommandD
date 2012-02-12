{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module F.CommandD.Filter.MacroFilter
( MacroFilter(..)
, ModeM
, aliasDev
, aliasKey
, command
, enableMode
, enableXMode
, newMacroFilter
, mode
, runMode
) where

{- ########################################################################################## -}
import            Control.Concurrent (forkIO)
import            Control.Concurrent.MVar
import            Control.Monad (forM_, replicateM_, when)
import            Control.Monad.Trans.State (State(..), StateT(..), evalStateT)
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

data Mode = Mode
  { modeChildren  :: [Mode]
  , modeEnabled   :: IORef Bool
  , modeName      :: ByteString
  , modeRootNode  :: Node
  }
  
type ModeM a  = StateT ModeS  IO a 
data ModeS    = ModeS KeyMap Mode

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

enableMode :: Sink MacroFilter -> [ByteString] -> Bool -> IO ()
enableMode mf name e = withMode mf name $ \m -> writeIORef (modeEnabled m) e

enableMode' :: Mode -> Bool -> IO ()
enableMode' m e = writeIORef (modeEnabled m) e

enableXMode :: Sink MacroFilter -> [ByteString] -> ByteString -> IO ()
enableXMode mf path name = withMode mf path $ \mode -> do
  forM_ (modeChildren mode) $ \m -> enableMode' m (modeName m == name)

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

newMode :: ByteString -> IO Mode
newMode name = do
  enabled <- newIORef True
  return $ Mode
    { modeChildren  = []
    , modeEnabled   = enabled
    , modeName      = name
    , modeRootNode  = defaultNode
    }
    
mode :: ByteString -> ModeM a -> ModeM a
mode name m = StateT $ \(ModeS kmap mode0) -> do
  newMode0              <- newMode name
  (a, ModeS _ newMode1) <- runStateT m $ ModeS kmap newMode0
  return (a, ModeS kmap (addMode mode0 newMode1))

runMode :: Filter MacroFilter -> ModeM a -> CD a
runMode (Sink f) m = lift $ modifyMVar f $ \dat0 -> do
  (a, ModeS k mode1)  <- runStateT m $ ModeS (mfsKeyMap dat0) (mfsRootMode dat0)
  modes               <- collectModes mode1
  return (dat0 
    { mfsKeyMap     = k
    , mfsModes      = modes
    , mfsNodes      = map modeRootNode modes
    , mfsRootMode   = mode1 
    }, a)

printMode :: Int -> Mode -> IO ()
printMode i mode = do
  replicateM_ i $ putStr "  "
  putStrLn $ "<" ++ (B.unpack $ modeName mode) ++ ">"
  forM_ (modeChildren mode) $ printMode (i + 1)
  printNode (i + 1) (modeRootNode mode)

withMode :: Filter MacroFilter -> [ByteString] -> (Mode -> IO ()) -> IO ()
withMode (Sink mf) path m = modifyMVar_ mf $ \s -> do 
  case findMode (mfsRootMode s) path of
    Just mode -> m mode >> resetModes' s
    Nothing   -> return s

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
  when ((not $ isLeafNode node) && (keyFlag (nodeKey node) == 0)) $ do
    appendNode node
  case (nodeAction node) of
    Just act  -> appendAction act
    Nothing   -> return ()
  
testNode depth node = 
  forM_ (nodeChildren node) $ \n -> do
    isKeyPressed (nodeKey n) >>= \p -> when p $ do
      testNode (depth - 1) n

testNodes :: MacroM ()
testNodes = removeNodes $ \nodes -> do
  depth <- pressedKeyCount
  forM_ nodes $ testNode depth

removeNodes :: ([Node] -> MacroM a) -> MacroM a
removeNodes fun = S.get >>= \s0 -> do
  S.put s0 { mfsNodes = [] }
  fun $ mfsNodes s0

resetNodes :: MacroM Bool
resetNodes = do
  s0 <- S.get
  let (n1, r)   = mod (mfsActions s0) (mfsNodes s0)
      mod [] [] = (map modeRootNode $ mfsModes s0, False)
      mod _  [] = (map modeRootNode $ mfsModes s0, True)
      mod _  n  = (n, True)
  S.put s0 { mfsNodes = n1 }
  return r

{- ########################################################################################## -}
  
--type MacroM a = State MacroFilterS a
type MacroM a = StateT MacroFilterS IO a

appendAction :: IO () -> MacroM ()
appendAction action = S.modify $ \s -> s { mfsActions = action:(mfsActions s) }

appendEvent :: Event -> CE ()
appendEvent e1 = ContST $ \e0 c -> c e0 () >> c e1 ()

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
  f <- resetNodes
  return f

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
  forM_ (mfsActions state1) $ \act -> forkIO $ act
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
 
aliasDev :: SourceId -> ByteString -> ModeM ()
aliasDev sid name = S.modify $ \(ModeS kmap mode) -> (ModeS (addDev kmap name sid) mode)
 
aliasKey :: ByteString -> ByteString -> ByteString -> ModeM ()
aliasKey key0 dev1 key1 = S.get >>= \(ModeS kmap mode) -> do
  let key = lookupKey kmap key0
      dev = lookupDev kmap dev1
  case (dev, key) of
    (Just d,  Just k) -> S.put $ ModeS (addKey kmap key1 d (keyCode k)) (mode)
    (d,       k     ) -> lift $ putStrLn $ concat ["[ERROR] Invalid key alias", show d, show k]

command :: String -> IO () -> ModeM ()
command combo action = S.get >>= \(ModeS kmap mode0) -> do 
  case parseMacro kmap combo of
    Left  err -> lift $ putStrLn err
    Right seq -> S.put $ ModeS kmap (addMacro mode0 action seq)
  
newMacroFilter :: CD (Filter MacroFilter)
newMacroFilter = lift $ do
  mode  <- newMode ""
  ref   <- newMVar MacroFilterS
    { mfsActions      = []
    , mfsFilteredKeys = []
    , mfsKeyMap       = defaultKeyMap
    , mfsModes        = []
    , mfsNodes        = []
    , mfsPressedKeys  = []
    , mfsRootMode     = mode
    , mfsSupressed    = []
    }
  return $ Sink ref
  
{- ########################################################################################## -}

alter :: (Maybe a -> a) -> (a -> Bool) -> [a] -> [a]
alter mod _    []                   = [mod Nothing]
alter mod pred (a:rest) | pred a    = (mod $ Just a):rest
                        | otherwise = a:(alter mod pred rest)
  
{- ########################################################################################## -}
