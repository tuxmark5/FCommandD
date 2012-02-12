{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module F.CommandD.Observer.ProcessObserver
( Environment
, ProcessEvent(..)
, ProcessObserver(..)
, getProcCmdLine
, getProcEnv
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
import            F.CommandD.Util.Chan
import            F.CommandD.Util.Directory
import            System.INotify hiding (Event)
import qualified  System.INotify as I
import            System.IO
{- ########################################################################################## -}

type Environment = [(ByteString, ByteString)]

data ProcessEvent 
  = ProcessCreated    Int
  | ProcessDestroyed  Int

data ProcessObserver = ProcessObserver
  { pmChan        :: ChanO ProcessEvent
  , pmWakeVar     :: MVar ()
  , pmWatchDescr  :: WatchDescriptor
  }

eventHandler :: MVar () -> I.Event -> IO ()
eventHandler syncVar _ = tryPutMVar syncVar () >> return ()

handle :: a -> IO a -> IO a
handle a m = E.handle (\(_ :: E.IOException) -> return a) m
  
getProcCmdLine :: Int -> IO [ByteString]
getProcCmdLine pid = handle [] $ do
  cmdline <- readFile' $ concat ["/proc/", show pid, "/cmdline"]
  return $ B.split 0 cmdline

getProcEnv :: Int -> IO Environment
getProcEnv pid = handle [] $ do
  dat <- readFile' $ concat ["/proc/", show pid, "/environ"]
  return $ map1 (dropSnd . B.breakByte 0x3D) (B.split 0 dat)
  where dropSnd (a, b) = (a, B.tail b)

getProcesses :: IO [Int]
getProcesses = mapDir "/proc" $ \f -> return . readMaybe

monitorLoop :: ProcessObserver -> [Int] -> IO ()
monitorLoop pm pids0 = do
  takeMVar (pmWakeVar pm)
  threadDelay 100000
  pids1 <- getProcesses >>= return . sort
  let (gone, _, new) = diff pids0 pids1
  forM_ gone $ writeChan (pmChan pm) . ProcessDestroyed
  forM_ new  $ writeChan (pmChan pm) . ProcessCreated
  threadDelay 2000000
  monitorLoop pm pids1
  
newProcessObserver :: INotify -> IO (ProcessObserver, ChanI ProcessEvent)
newProcessObserver inotify = do
  (chanI, chanO)  <- newChan
  wakeVar         <- newEmptyMVar
  watchDescr      <- addWatch inotify [Open] "/lib64/ld-linux-x86-64.so.2" (eventHandler wakeVar)
  let pm = ProcessObserver chanO wakeVar watchDescr
  forkIO $ monitorLoop pm []
  return (pm, chanI)
  
{- ########################################################################################## -}

diff :: Ord a => [a] -> [a] -> ([a], [a], [a])
diff l0      []      = (l0, [], [])
diff []      l1      = ([], [], l1)
diff (a0:r0) (a1:r1) = case compare a0 a1 of
  LT -> (\(a, b, c) -> (a0:a,    b,    c)) $ diff     r0  (a1:r1)
  EQ -> (\(a, b, c) -> (   a, a0:b,    c)) $ diff     r0      r1
  GT -> (\(a, b, c) -> (   a,    b, a1:c)) $ diff (a0:r0)     r1

map1 :: (a -> b) -> [a] -> [b]
map1 _ []     = []
map1 _ [_]    = []
map1 f (x:xs) = f x : map1 f xs
  
readFile' :: FilePath -> IO ByteString
readFile' f = openBinaryFile f ReadMode >>= B.hGetContents

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing
        
{- ########################################################################################## -}
