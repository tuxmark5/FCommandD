{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, forM_, when)
import F.CommandD.Conn
import F.CommandD.Daemon
import F.CommandD.Filter.DebugFilter
import F.CommandD.Filter.HubFilter
import F.CommandD.Filter.MacroFilter
import F.CommandD.Sink
import F.CommandD.Sink.UInputSink
import F.CommandD.Source
import F.CommandD.Source.EVDevSourceDyn
import System.IO
import System.Linux.Input

import F.CommandD.ContST
import F.CommandD.Monitor.ProcessMonitor
import F.CommandD.Monitor.SessionMonitor

-- Xorg: query active window
-- Xorg: listen of X servers
-- Dbus: listen for dbus sessions


mdev :: EVDevMatcher
mdev 0x1532 0x001f  = True      -- Naga
mdev _      _       = False

main :: IO ()
main = withINotify $ \inotify -> daemon $ do
  evdev       <- mkEVDevSourceDyn mdev
  uinput0     <- mkUInputSink "UInput: Primary"
  uinput1     <- mkUInputSink "UInput: DisplayLink"
  macro       <- mkMacroFilter
  debug       <- mkDebugFilter
  hub         <- mkHub
  
  (pmon, pc)  <- lift $ newProcessMonitor inotify
  (smon, sc)  <- lift $ newSessionMonitor pc ["dwm", "xfce4-session"] 
  
  runMode macro $ do
    mode "firefox" $ do
      command "+1+2" $ do
        lift $ runAs smon ":0" "xterm" []
      command "+1+3" $ do
        lift $ runAs smon ":5" "xterm" []
  
  evdev >>> macro >>> hub
  --evdev >>> macro >>> debug >>> hub 
  hubSetSink hub uinput0
  
  lift $ threadDelay 500000000
