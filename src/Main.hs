{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, forM_, when)
import Data.Word (Word16)
import F.CommandD.Conn
import F.CommandD.Daemon
import F.CommandD.Filter.DebugFilter
import F.CommandD.Filter.HubFilter
import F.CommandD.Filter.MacroFilter
import F.CommandD.Sink
import F.CommandD.Sink.UInputSink
import F.CommandD.Source

import System.IO
import System.Linux.Input

import            F.CommandD.Action.Guayadeque
import            F.CommandD.Action.XMonad
import            F.CommandD.Source.EVDevSourceDyn
import            F.CommandD.Util.Commander
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as B
import            F.CommandD.Observer.SessionObserver

import            F.CommandD.Observer.FocusObserver

-- TODO: EVDev hotplug
-- TOOD: Session termination
-- TODO: event simulation

{-
  NOTES:
  * add "Defaults exempt_group=root" into sudoers
  * add dbus policy for root
-}

{- ########################################################################################## -}

-- Select input devices
mdev :: Word16 -> Word16 -> IO SourceId
mdev 0x1532 0x010d  = return 0      -- BlackWidow
mdev 0x1532 0x001f  = return 1      -- Naga
mdev _      _       = return (-1)

-- Name sessions based on DISPLAY environment variable
sesId :: Session -> IO ByteString
sesId Session { sesDisplay = ":5" } = return "Dl"
sesId _                             = return "Main"

{- ########################################################################################## -}

-- rename Razer BlackWidow M1-M5 keys
bwidKeys0 = ["1", "2", "3", "4", "5"]
bwidKeys1 = map (\i -> "M" ++ (show i)) [1..5]
-- rename Razer Naga Epic 1-12 keys
nagaKeys0 = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "Minus", "Equal"]
nagaKeys1 = map (\i -> "N" ++ (show i)) [1..12]

-- rename keys
registerKeys :: ModeM ()
registerKeys = do
  aliasDev 0 "B" 
  aliasDev 1 "N" 
  aliasKey "LeftMeta" "B" "Super"
  aliasKey "CapsLock" "B" "Hyper"
  forM_ (zip bwidKeys0 bwidKeys1) $ \(k0, k1) -> aliasKey k0 "B" (B.pack k1)
  forM_ (zip nagaKeys0 nagaKeys1) $ \(k0, k1) -> aliasKey k0 "N" (B.pack k1)
  
{- ########################################################################################## -}
  
-- mode to launch applications
modeApps :: (String -> [String] -> CD ()) -> ModeM ()
modeApps run = do
  command "*LeftAlt+F2"       $ run "`dmenu_path | dmenu -b`"     []
  command "*Super+B"          $ run "VirtualBox"                  []
  command "*Super+C"          $ run "urxvt -e python"             []
  command "*Super+F*Super+F"  $ run "firefox"                     []
  command "*Super+F*Super+M"  $ run "Thunar"                      []
  command "*Super+H"          $ run "krusader --left ~ --right ~" []
  command "*Super+O"          $ run "opera"                       []
  command "*Super+R"          $ run "transmission-gtk"            []
  command "*Super+S"          $ run "skype"                       []
  command "*Super+T"          $ run "urxvt -e tmux"               []
  command "*Super+Y"          $ run "pkexec synaptic"             []
  command "*Super+V"          $ run "vmware"                      []

-- mode to control Guayadeque via DBus
modeGuayadeque :: (MethodCall -> CD ()) -> ModeM ()
modeGuayadeque call = do
  command "+NextSong"         $ call $ gdqNext
  command "+PlayPause"        $ call $ gdqPlayPause
  command "+PreviousSong"     $ call $ gdqPrev
  command "+StopCD"           $ call $ gdqStop

  command "*Hyper+N10"        $ call $ gdqPrev
  command "*Hyper+N11"        $ call $ gdqPlayPause
  command "*Hyper+N12"        $ call $ gdqNext
  
-- mode to control XMonad via DBus
modeXMonad :: (MethodCall -> CD ()) -> ModeM ()
modeXMonad call = do
  command "*Hyper+Z"          $ call $ xmonadCoreExit
  command "*Hyper+X"          $ call $ xmonadCoreRestart
  command "*Hyper+H"          $ call $ xmonadCoreSetWMName "L3GD"
  command "*Hyper+K"          $ call $ xmonadLayoutExpand
  command "*Hyper+Backslash"  $ call $ xmonadLayoutNext
  command "*Hyper+J"          $ call $ xmonadLayoutShrink
  command "*Hyper+Space"      $ call $ xmonadMasterFocus
  command "*Hyper+Minus"      $ call $ xmonadMasterMod (-1)
  command "*Hyper+Equal"      $ call $ xmonadMasterMod ( 1)
  command "*Hyper+Enter"      $ call $ xmonadMasterSwap
  command "*Hyper+Grave"      $ call $ xmonadNavGridSelect
  command "*Hyper+C"          $ call $ xmonadWinClose
  command "*Hyper+T"          $ call $ xmonadWinSink
  
  forM_ (zip "1234QWERASDF" "123456789ABC") $ \(key, wk) -> do
    command ("*Hyper+"         ++ [key]) $ call $ xmonadWkSetCurrent [wk]
    command ("*Hyper*LeftAlt+" ++ [key]) $ call $ xmonadWkMoveWindow [wk]
  
{- ########################################################################################## -}

main :: IO ()
main = daemon $ do
  lift $ putStrLn "[*] Starting ..."
  evdev       <- mkEVDevSourceDyn mdev
  uinput0     <- mkUInputSink "UInput: Primary"
  uinput1     <- mkUInputSink "UInput: DisplayLink"
  macro       <- mkMacroFilter
  debug       <- mkDebugFilter
  
  (cmd, hub)  <- newCommander sesId $ do
    addSink "Main"  uinput0
    addSink "Dl"    uinput1

  call        <- getCallCmd cmd
  run         <- getRunCmd cmd
  
  runMode macro $ do
    registerKeys
    command "+N6" $ nextProfile cmd
    mode "apps"         $ modeApps run
    mode "guayadeque"   $ modeGuayadeque call
    mode "xmonad"       $ modeXMonad call
  
  evdev >>> macro >>> hub
  lift $ putStrLn "[*] Initialized ..."
  lift $ forever $ threadDelay 500000000

{- ########################################################################################## -}
