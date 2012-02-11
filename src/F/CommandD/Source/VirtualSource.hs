module F.CommandD.Source.VirtualSource
( module System.Linux.Keys
, delay
, down
, downUp
, hold
, runMacro
, up
) where

{- ########################################################################################## -}
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.State
import Data.Int  (Int32, Int64)
import Data.Word (Word16, Word32)
import F.CommandD.ContST (ContST(..))
import F.CommandD.Core
import F.CommandD.Sink
import System.Linux.Input
import System.Linux.Keys
{- ########################################################################################## -}

type MacroM a = StateT MacroS IO a

data MacroS = MacroS
  { macroInter    :: IO ()
  , macroSink     :: CE ()
  , macroTimeSec  :: Int64
  , macroTimeUSec :: Int64
  }

{- ########################################################################################## -}

delay :: Int -> MacroM ()
delay d = do
  lift $ threadDelay (d * 1000)
  vdelay d

down :: Word16 -> MacroM ()
down k = do
  newEvent evKEY k 1 >>= sendEvent
  newSynEvent        >>= sendEvent

downUp :: Word16 -> MacroM ()
downUp k = down k >> up k

hold :: Word16 -> MacroM a -> MacroM a
hold k m = down k >> m >>= \r -> up k >> return r

up :: Word16 -> MacroM ()
up k = do
  newEvent evKEY k 0 >>= sendEvent
  newSynEvent        >>= sendEvent

runMacro :: SinkC c => Sink c -> MacroM a -> CD a
runMacro (Sink s) m = lift $ runMacroM (sinkWrite s) m

runMacroM :: CE () -> MacroM a -> IO a
runMacroM sink m = do
  time <- getTimeOfDay
  evalStateT m MacroS
    { macroInter    = return ()
    , macroSink     = sink
    , macroTimeSec  = timevalSec  time
    , macroTimeUSec = timevalUSec time
    }
    
vdelay :: Int -> MacroM ()
vdelay d = modify $ \s -> s { macroTimeUSec = macroTimeUSec s + (fromIntegral d) * 1000000 }

{- ########################################################################################## -}

getLift :: (MacroS -> IO a) -> MacroM a
getLift k = StateT $ \s -> k s >>= \r -> return (r, s)

newEvent :: Word16 -> Word16 -> Int32 -> MacroM Event
newEvent t c v = get >>= \s -> return Event
  { eventTime   = Timeval 
    { timevalSec  = macroTimeSec s
    , timevalUSec = macroTimeUSec s
    }
  , eventType   = t
  , eventCode   = c
  , eventValue  = v
  , eventSource = 0
  }

newSynEvent :: MacroM Event
newSynEvent = newEvent evSYN 0 0

sendEvent :: Event -> MacroM ()                                         
sendEvent e = getLift $ \s -> sendEvent' (macroSink s) e
                                         
sendEvent' :: CE () -> Event -> IO ()  
sendEvent' m e0 = runContST m e0 (\e1 _ -> return (e1, ())) >> return ()

{- ########################################################################################## -}
