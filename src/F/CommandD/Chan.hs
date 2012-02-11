module F.CommandD.Chan
( ChanI
, ChanO
, dupChanI
, newChan
, newChanO
, readChan
, writeChan
) where
  
{- ########################################################################################## -}
import            Control.Concurrent.MVar
{- ########################################################################################## -}

data ChanI a = ChanI (MVar (Stream a))
data ChanO a = ChanO (MVar (Stream a))

type Stream a = MVar (ChItem a)
data ChItem a = ChItem a (Stream a)

{- ########################################################################################## -}

dupChanI :: ChanO a -> IO (ChanI a)
dupChanI (ChanO writeVar) = do
  hole       <- readMVar writeVar
  newReadVar <- newMVar hole
  return (ChanI newReadVar)

newChan :: IO (ChanI a, ChanO a)
newChan = do
  hole      <- newEmptyMVar
  readVar   <- newMVar hole
  writeVar  <- newMVar hole
  return (ChanI readVar, ChanO writeVar)

newChanO :: IO (ChanO a)
newChanO = do
  hole      <- newEmptyMVar  
  writeVar  <- newMVar hole
  return (ChanO writeVar)


readChan :: ChanI a -> IO a
readChan (ChanI readVar) = do
  modifyMVar readVar $ \readEnd -> do
    (ChItem val newReadEnd) <- readMVar readEnd
    return (newReadEnd, val)

writeChan :: ChanO a -> a -> IO ()
writeChan (ChanO writeVar) val = do
  newHole <- newEmptyMVar
  modifyMVar_ writeVar $ \oldHole -> do
    putMVar oldHole (ChItem val newHole)
    return newHole
   
{- ########################################################################################## -}
