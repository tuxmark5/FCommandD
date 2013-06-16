module F.CommandD.Observer.ProcessObserver
( Environment
, Process(..)
, ProcessEvent(..)
, ProcessObserver(..)
, getProcCmdLine
, getProcEnv
, getProcUid
, getProcesses
, isProcessRunning
, newProcessObserver
, withINotify
) where
  
{- ########################################################################################## -}
import            Control.Concurrent (forkIO, threadDelay)
import            Control.Concurrent.MVar
import qualified  Control.Exception as E
import            Control.Monad
import            Data.ByteString (ByteString)
import qualified  Data.ByteString as B
import            Data.IORef
import            Data.List (sort)
import            Data.Maybe (catMaybes)
import            F.CommandD.Util.Chan
import            F.CommandD.Util.Directory
import            System.INotify hiding (Event)
import qualified  System.INotify as I
import            System.IO
import            System.Posix (CUid)
import            System.Posix.Files (fileOwner, getFileStatus)
{- ########################################################################################## -}

type Environment  = [(ByteString, ByteString)]

data Process      = Process
  { procPid       :: Int
  , procCmd       :: [ByteString]
  } deriving (Show)

data ProcessEvent 
  = ProcessCreated    Process
  | ProcessDestroyed  Process

data ProcessObserver = ProcessObserver
  { poChan        :: ChanO ProcessEvent
  , poProcesses   :: IORef [Process]
  , poWakeVar     :: MVar ()
  , poWatchDescr  :: [WatchDescriptor]
  }

instance Eq   Process where (==)    (Process a _) (Process b _) = a == b
instance Ord  Process where compare (Process a _) (Process b _) = compare a b

{- ########################################################################################## -}

-- infixr 1 +>>
infixl 1 >>+
(>>+) :: Monad m => m a -> (a -> b) -> m b
(>>+) m f = m >>= return . f

getProcesses :: ProcessObserver -> IO [Process]
getProcesses po = readIORef $ poProcesses po

isProcessRunning :: ProcessObserver -> ByteString -> IO Bool
isProcessRunning po name = getProcesses po >>= return . any filt where
  filt (Process _ (name':_))  = name == name'
  filt _                      = False

monitorLoop :: ProcessObserver -> [Process] -> IO ()
monitorLoop pm proc0 = do
  takeMVar (poWakeVar pm)
  threadDelay 100000
  proc1 <- queryProcesses >>+ map (\pid -> Process pid []) . sort
  let (gone, same, new) = diff proc0 proc1
  new'  <- mapM initProcess new
  let proc2 = merge same new'
  forM_ gone $ writeChan (poChan pm) . ProcessDestroyed
  forM_ new' $ writeChan (poChan pm) . ProcessCreated
  writeIORef (poProcesses pm) proc2
  threadDelay 2000000
  monitorLoop pm proc2
  
newProcessObserver :: INotify -> IO (ProcessObserver, ChanI ProcessEvent)
newProcessObserver inotify = do
  (chanI, chanO)  <- newChan
  procs           <- newIORef []
  wakeVar         <- newEmptyMVar
  let addW file    = addWatch inotify [Open] file (eventHandler wakeVar)
  watchDescr      <- mapCatch addW ["/lib/ld-linux.so.2", "/lib64/ld-linux-x86-64.so.2"]
  let pm = ProcessObserver chanO procs wakeVar watchDescr
  forkIO $ monitorLoop pm []
  tryPutMVar wakeVar ()
  return (pm, chanI)

{- ########################################################################################## -}

eventHandler :: MVar () -> I.Event -> IO ()
eventHandler syncVar _ = tryPutMVar syncVar () >> return ()

getProcCmdLine :: Int -> IO [ByteString]
getProcCmdLine pid = handle [] $ do
  cmdline <- readFile' $ concat ["/proc/", show pid, "/cmdline"]
  return $ B.split 0 cmdline

getProcEnv :: Int -> IO Environment
getProcEnv pid = handle [] $ do
  dat <- readFile' $ concat ["/proc/", show pid, "/environ"]
  return $ map1 (dropSnd . B.breakByte 0x3D) (B.split 0 dat)
  where dropSnd (a, b) = (a, B.tail b)

getProcUid :: Int -> IO CUid
getProcUid pid = handle 1000 $ do
  stat <- getFileStatus $ concat ["/proc/", show pid, "/cmdline"]
  return $ fromIntegral $ fileOwner stat

initProcess :: Process -> IO Process
initProcess (Process pid _) = getProcCmdLine pid >>+ Process pid

queryProcesses :: IO [Int]
queryProcesses = mapDir "/proc" $ \f -> return . readMaybe

{- ########################################################################################## -}

diff :: Ord a => [a] -> [a] -> ([a], [a], [a])
diff l0      []      = (l0, [], [])
diff []      l1      = ([], [], l1)
diff (a0:r0) (a1:r1) = case compare a0 a1 of
  LT -> (\(a, b, c) -> (a0:a,    b,    c)) $ diff     r0  (a1:r1)
  EQ -> (\(a, b, c) -> (   a, a0:b,    c)) $ diff     r0      r1
  GT -> (\(a, b, c) -> (   a,    b, a1:c)) $ diff (a0:r0)     r1

handle :: a -> IO a -> IO a
handle a m = E.handle (\(_ :: E.IOException) -> return a) m

map1 :: (a -> b) -> [a] -> [b]
map1 _ []     = []
map1 _ [_]    = []
map1 f (x:xs) = f x : map1 f xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge l0     []     = l0
merge []     l1     = l1
merge (a:ar) (b:br) = case compare a b of
  LT -> a:merge    ar   (b:br)
  EQ -> a:merge    ar   (b:br) 
  GT -> b:merge (a:ar)     br

mapCatch :: (a -> IO b) -> [a] -> IO [b]
mapCatch k list = sequence (map fun list) >>= return . catMaybes
  where fun a = E.catch (liftM return $ k a) $ \(E.SomeException e) -> return Nothing
  
readFile' :: FilePath -> IO ByteString
readFile' f = openBinaryFile f ReadMode >>= B.hGetContents

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing
        
{- ########################################################################################## -}
