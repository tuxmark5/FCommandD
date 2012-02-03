{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when)
import F.CommandD.Conn
import F.CommandD.Daemon
import F.CommandD.Filter.HubFilter
import F.CommandD.Filter.MacroFilter
import F.CommandD.Sink
import F.CommandD.Sink.UInputSink
import F.CommandD.Source
import F.CommandD.Source.EVDevSourceDyn
import System.IO
import System.Linux.Input
import F.CommandD.ContST

{-


daemon  :: Daemon a -> IO a
mode    :: String -> Mode a -> Daemon a
command :: String -> Command a -> Mode a

modeEnable :: String -> Bool -> Command ()


data Key = Key
  { keyDevice     :: Device
  , keyCode       :: Word16
  }

  
  
data MacroFilterS 
  
data Mode
  
data Node = Node 
  { nodeAction    :: Command ()
  , nodeChildren  :: MVar Map Key Node
  , nodeKey       :: Key
  }
  

multiple layers of connections, one for mouse events, one for keyboard;
use the bits?
  
type CD a = StateT (MVar DaemonS) IO a

-- TCPSink, TCPSource
 -}

  -- CD, CM, CC
  
  
{-


hub :: CE ()
hub = [localSplit1, tcp, localSplit2]

tcp   <- mkTCPServer
evdev <- mkEVDevDevice
filt  <- mkMacroFilter
hub   <- mkHub

[evdev, tcp] >>> filt >>> hub

conn All [tcp, evdev] filt
conn All [filt, virt] hub
conn All hub uinput

t
tcp server can have hooks, which connect stuff


-}
  
-- runSink
-- runSource
{-
fw :: (Source src, Sink sink) => src -> sink -> CD ()
fw src sink = forever $ do
  e <- sourceRead src
  lift $ do
    x <- getClockTime
    putStrLn $ show $ eventTime e
    putStrLn $ show $ x 
  --lift $ putStrLn $ "EVT " ++ (show $ eventType e) ++ " " ++ (show $ eventCode e) ++ " "
  --  ++ (show $ eventValue e)
  sinkWrite sink e
  --sinkWrite sink2 e
  return ()
  -}

mdev :: EVDevMatcher
mdev 0x1532 0x001f  = True
mdev _      _       = False
  
main :: IO ()
main = daemon $ do
  evdev   <- mkEVDevSourceDyn mdev
  uinput0 <- mkUInputSink "UInput: Primary"
  uinput1 <- mkUInputSink "UInput: DisplayLink"
  hub     <- hubNew
  
  evdev >>> hub
  
  hubSetSink hub uinput1
  lift $ threadDelay 10000000

