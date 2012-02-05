{-# LANGUAGE TypeSynonymInstances #-}

module F.CommandD.Filter.MacroFilter
( MacroFilter(..)
, command
, mkMacroFilter
, mode
, runMode
) where

{- ########################################################################################## -}
import            Control.Concurrent (forkIO)
import            Control.Monad (forM_, replicateM_, when)
import            Control.Monad.Trans.State (State(..), StateT(..), evalStateT)
import qualified  Control.Monad.Trans.State as S
import            Data.Int (Int32)
import            Data.List (partition)
import            Data.IORef
import            F.CommandD.ContST
import            F.CommandD.Daemon
import            F.CommandD.Filter
import            F.CommandD.Filter.MacroParser
import            F.CommandD.Sink
import            F.CommandD.Util.KeyMap
import            System.Linux.Input
{- ########################################################################################## -}

type MacroFilter = IORef MacroFilterS
  
data MacroFilterS = MacroFilterS
  { mfsActions      :: [CD ()] 
  , mfsFilteredKeys :: [Key]
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
      _ | et == evMSC -> exitContST () --supressEvent var
        | et == evREL -> return () -- appendEvent $ synEvent e
        | et == evSYN -> return () -- exitContST ()
        | et == evKEY -> do
          filt <- runMacroM var (processKeyEvent e) 
          if filt
            then exitContST ()
            else appendEvent $ synEvent e

{- ########################################################################################## -}
  
--type MacroM a = State MacroFilterS a
type MacroM a = StateT MacroFilterS IO a

appendAction :: CD () -> MacroM ()
appendAction action = S.modify $ \s -> s { mfsActions = action:(mfsActions s) }

appendEvent :: Event -> CE ()
appendEvent e1 = ContST $ \e0 c -> c e0 () >> c e1 ()

appendNode :: Node -> MacroM ()
appendNode node = S.modify $ \s -> s { mfsNodes = node:(mfsNodes s) }

isKeyFiltered :: Key -> MacroM Bool
isKeyFiltered key = S.gets mfsFilteredKeys >>= return . any (== key) 

isKeyPressed :: Key -> MacroM Bool
isKeyPressed key = S.gets mfsPressedKeys >>= return . any (== key) 

isLeafNode :: Node -> Bool
isLeafNode node = null $ nodeChildren node

modifyFilteredKeys :: ([Key] -> [Key]) -> MacroM ()
modifyFilteredKeys mod = S.modify $ \s -> s { mfsFilteredKeys = mod $ mfsFilteredKeys s }

modifyNodes :: ([Node] -> ([Node], a)) -> MacroM a
modifyNodes mod =  S.get >>= \s0 -> do
  let (n1, r) = mod $ mfsNodes s0
  S.put s0 { mfsNodes = n1 }
  return r

modifyPressedKeys :: ([Key] -> [Key]) -> MacroM ()
modifyPressedKeys mod = S.modify $ \s -> s { mfsPressedKeys = mod $ mfsPressedKeys s }
   
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

removeFilteredKey :: Key -> MacroM Bool
removeFilteredKey k = do
  st0 <- S.get
  let (r, fkeys1) = partition (== k) (mfsFilteredKeys st0)
  S.put st0 { mfsFilteredKeys = fkeys1 }
  return $ not $ null r

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
  
runMacroM :: MacroFilter -> MacroM a -> CE a
runMacroM var m = do
  state0          <- lift $ readIORef var
  --let (r, state1)  = S.runState m state0
  (r, state1)     <- lift $ S.runStateT m state0
  lift $ do
    writeIORef var state1 { mfsActions = [] }
    forM_ (mfsActions state1) $ \act -> forkIO $ do
      (_, _) <- runStateT act Daemon
      return ()
  return r
  
supressEvent :: MacroFilter -> CE ()
supressEvent var = saveContST $ \s -> do
  lift $ modifyIORef var $ \d -> d { mfsSupressed = s:(mfsSupressed d) }
  exitContST ()
  
synEvent :: Event -> Event
synEvent base   = base
  { eventType   = evSYN
  , eventCode   = 0
  , eventValue  = 0
  }
  
testNode :: Int -> Node -> MacroM ()
testNode 0 _        = return ()
testNode depth node = 
  forM_ (nodeChildren node) $ \n -> do
    isKeyPressed (nodeKey n) >>= \p -> when p $ do
      when ((not $ isLeafNode n) && (keyFlag (nodeKey n) == 0)) $ do
        appendNode n
      case (nodeAction n) of
        Just act  -> appendAction act
        Nothing   -> return ()
      testNode (depth - 1) n

testNodes :: MacroM ()
testNodes = removeNodes $ \nodes -> do
  depth <- pressedKeyCount
  forM_ nodes $ testNode depth
  
toKey :: Event -> Key
toKey e = defaultKey { keyCode = fromIntegral $ eventCode e }

updatePressedKeys :: Event -> Key -> MacroM ()
updatePressedKeys e key = modifyPressedKeys $ case (eventValue e) of
  0 -> filter (/= key)
  1 -> (key :)
  2 -> id

{- ########################################################################################## -}

data Mode = Mode
  { modeChildren  :: [Mode]
  , modeName      :: String
  , modeRootNode  :: Node
  }
  
type ModeM a  = StateT ModeS  IO a 
data ModeS    = ModeS KeyMap Mode

data Node = Node
  { nodeAction    :: Maybe (CD ())
  , nodeChildren  :: [Node]
  , nodeKey       :: Key
  }

printMode :: Int -> Mode -> IO ()
printMode i mode = do
  replicateM_ i $ putStr "  "
  putStrLn $ "<" ++ (modeName mode) ++ ">"
  forM_ (modeChildren mode) $ printMode (i + 1)
  printNode (i + 1) (modeRootNode mode)
  
printNode :: Int -> Node -> IO ()
printNode i node = do
  replicateM_ i $ putStr "  "
  putStrLn $ "[" ++ (show $ nodeKey node) ++ "]"
  forM_ (nodeChildren node) $ printNode (i + 1)
  
{- ########################################################################################## -}
 
addMacro :: Mode -> CD () -> [Key] -> Mode
addMacro m0 act combo = m0 { modeRootNode = addMacro' (modeRootNode m0) act combo }

alter :: (Maybe a -> a) -> (a -> Bool) -> [a] -> [a]
alter mod _    []                   = [mod Nothing]
alter mod pred (a:rest) | pred a    = (mod $ Just a):rest
                        | otherwise = a:(alter mod pred rest)

addMacro' :: Node -> CD () -> [Key] -> Node
addMacro' n0 act []           = n0 { nodeAction   = Just act }
addMacro' n0 act (first:rest) = n0 { nodeChildren = alter f test (nodeChildren n0) }
  where f (Just n)    = addMacro' n  act rest
        f (Nothing)   = addMacro' n' act rest
        n'            = Node Nothing [] first
        test n        = nodeKey n == first
 
addMode :: Mode -> Mode -> Mode
addMode par cld = par { modeChildren = cld:modeChildren par }
 
command :: String -> CD () -> ModeM ()
command combo action = S.get >>= \(ModeS kmap mode0) -> do 
  case parseMacro kmap combo of
    Left  err -> lift $ putStrLn err
    Right seq -> do
      lift $ putStrLn $ "ADD SEQ:" ++ (show seq)
      S.put $ ModeS kmap (addMacro mode0 action seq)
  
defaultNode :: Node
defaultNode = Node
  { nodeAction    = Nothing
  , nodeChildren  = []
  , nodeKey       = defaultKey
  }
  
flattenModes :: Mode -> [Mode]
flattenModes mode = mode:(concat $ map flattenModes $ modeChildren mode)
  
mkMacroFilter :: CD (Filter MacroFilter)
mkMacroFilter = do
  ref <- lift $ newIORef MacroFilterS
    { mfsActions      = []
    , mfsFilteredKeys = []
    , mfsModes        = []
    , mfsNodes        = []
    , mfsPressedKeys  = []
    , mfsRootMode     = mkMode []
    , mfsSupressed    = []
    }
  return $ Sink ref
  
mkMode :: String -> Mode
mkMode name = Mode
  { modeChildren  = []
  , modeName      = name
  , modeRootNode  = defaultNode
  }
  
mode :: String -> ModeM a -> ModeM a
mode name m = StateT $ \(ModeS kmap mode0) -> do
  (a, ModeS _ newMode) <- runStateT m $ ModeS kmap (mkMode name)
  return (a, ModeS kmap (addMode mode0 newMode))

runMode :: Filter MacroFilter -> ModeM a -> CD a
runMode (Sink f) m = lift $ do
  dat0                <- readIORef f
  (a, ModeS _ mode1)  <- runStateT m $ ModeS defaultKeyMap (mfsRootMode dat0)
  writeIORef f dat0 
    { mfsModes      = flattenModes mode1
    , mfsRootMode   = mode1 
    }
  return a
  
{- ########################################################################################## -}
