{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import            Control.Concurrent (forkIO, threadDelay)
import            Control.Monad (forever, forM_, when)
import qualified  Data.ByteString.Char8 as B
import            F.CommandD.Daemon
import            F.CommandD.Action.Firefox
import            F.CommandD.Action.Guayadeque
import            F.CommandD.Action.XMonad
import            F.CommandD.Source.EVDevSourceDyn
import            F.CommandD.Source.VirtualSource
import            F.CommandD.Util.Commander
import            F.CommandD.Util.WindowMatcher
import            System.IO

{-
  NOTES:
  * add "Defaults exempt_group=root" into sudoers
  * add dbus policy for root

  TODO:
  * run outside session

  * evdev hotplug/reload
  * parse UID from environment
  * TCP source/sink
  
-}

{- ########################################################################################## -}

run :: String -> String
run = id

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
bwidKeys0 = ["F13", "F14", "F15", "F16", "F17"]
bwidKeys1 = ["M1" , "M2" , "M3" , "M4" , "M5" ]
-- rename Razer Naga Epic 1-12 keys
nagaKeys0 = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "Minus", "Equal"]
nagaKeys1 = map (\i -> "N" ++ (show i)) [1..12]

-- rename keys
registerKeys :: ModeM Commander ()
registerKeys = do
  aliasDev 0 "B" 
  aliasDev 1 "N" 
  aliasKey "LeftMeta" "B" "Super"
  aliasKey "CapsLock" "B" "Hyper"
  aliasKey "F24"      "B" "FN"
  forM_ (zip bwidKeys0 bwidKeys1) $ \(k0, k1) -> aliasKey k0 "B" (B.pack k1)
  forM_ (zip nagaKeys0 nagaKeys1) $ \(k0, k1) -> aliasKey k0 "N" (B.pack k1)
  
{- ########################################################################################## -}
  
-- mode to launch applications
modeApps :: ModeM Commander ()
modeApps = do
  command "*LeftAlt+F2"       $ run "`dmenu_path | dmenu -b`"
  command "*Super+B"          $ run "VirtualBox"
  command "*Super+C"          $ run "urxvt -e python"
  command "*Super+F*Super+F"  $ run "firefox"
  command "*Super+F*Super+M"  $ run "Thunar"
  command "*Super+H"          $ run "krusader --left ~ --right ~"
  command "*Super+O"          $ run "opera"
  command "*Super+R"          $ run "transmission-gtk"
  command "*Super+S"          $ run "skype"
  command "*Super+T"          $ run "urxvt -e tmux"
  command "*Super+Y"          $ run "pkexec synaptic"
  command "*Super+V"          $ run "vmware"

-- mode to control Guayadeque via DBus
modeGuayadeque :: ModeM Commander ()
modeGuayadeque = do
  command "+NextSong"         $ gdqNext
  command "+PlayPause"        $ gdqPlayPause
  command "+PreviousSong"     $ gdqPrev
  command "+StopCD"           $ gdqStop

  command "*Hyper+N10"        $ gdqPrev
  command "*Hyper+N11"        $ gdqPlayPause
  command "*Hyper+N12"        $ gdqNext

-- mode to control XMonad via DBus
modeXMonad :: ModeM Commander ()
modeXMonad = do
  command "*Hyper+Z"          $ xmonadCoreExit
  command "*Hyper+X"          $ xmonadCoreRestart
  command "*Hyper+H"          $ xmonadCoreSetWMName "L3GD"
  command "*Hyper+K"          $ xmonadLayoutExpand
  command "*Hyper+Backslash"  $ xmonadLayoutNext
  command "*Hyper+J"          $ xmonadLayoutShrink
  command "*Hyper+Space"      $ xmonadMasterFocus
  command "*Hyper+Minus"      $ xmonadMasterMod (-1)
  command "*Hyper+Equal"      $ xmonadMasterMod ( 1)
  command "*Hyper+Enter"      $ xmonadMasterSwap
  command "*Hyper+Grave"      $ xmonadNavGridSelect
  command "*Hyper+RightBrace" $ xmonadTabNext
  command "*Hyper+LeftBrace"  $ xmonadTabPrev
  command "*Hyper+U"          $ xmonadTabUnmerge
  command "*Hyper+C"          $ xmonadWinClose
  command "*Hyper+T"          $ xmonadWinSink

  -- command "+N1"               $ call $ xmonadWkSetCurrent "1"

  -- Workspace navigation
  forM_ (zip "1234QWERASDF" "123456789ABC") $ \(key, wk) -> do
    command ("*Hyper+"         ++ [key]) $ xmonadWkSetCurrent [wk]
    command ("*Hyper*LeftAlt+" ++ [key]) $ xmonadWkMoveWindow [wk]

  -- Screen navigation
  forM_ (zip3 ["Comma", "Dot"] ["N4", "N6"] [0, 1]) $ \(k0, k1, scr) -> do
    command ("*Hyper+"         ++ k0) $ xmonadScreenSetCurr scr
    command ("*Hyper*LeftAlt+" ++ k0) $ xmonadScreenMoveWin scr
    command ("*Hyper+"         ++ k1) $ xmonadScreenMoveWin scr

  -- Directional window navigation
  forM_ (zip ["L", "Apostrophe", "P", "SemiColon"] [0, 1, 2, 3]) $ \(key, dir) -> do
    command ("*Hyper+"         ++ key) $ xmonadNavMove dir
    command ("*Hyper*LeftAlt+" ++ key) $ xmonadTabMerge dir

{- ########################################################################################## -}

fastMouseWheel :: ModeM Commander ()
fastMouseWheel = do
  command "+WheelDown:N"      $ rel relWheel (-5)
  command "+WheelUp:N"        $ rel relWheel ( 5)

modeDefault :: ModeM Commander ()
modeDefault = mode "default" $ do
  fastMouseWheel

modeFirefox :: ModeM Commander ()
modeFirefox = mode "firefox" $ do
  command "+N1"               $ firefoxGoBack
  command "+N2"               $ firefoxTabClose
  command "+N3"               $ firefoxGoForward
  command "+N4"               $ firefoxOpenInNewTab
  command "*LeftShift+N4"     $ firefoxSearchForSel

modeGEdit :: ModeM Commander ()
modeGEdit = mode "gedit" $ do
  fastMouseWheel 

modeKrusader :: ModeM Commander ()
modeKrusader = mode "krusader" $ do
  return ()

modeMPlayer :: ModeM Commander ()
modeMPlayer = mode "mplayer" $ do
  return ()

modeOpera :: ModeM Commander ()
modeOpera = mode "opera" $ do
  return ()

modeSublime :: ModeM Commander ()
modeSublime = mode "sublime" $ do
  fastMouseWheel

modeVMware :: ModeM Commander ()
modeVMware = mode "vmware" $ do
  command "+Hyper" $ hold keyLeftCtrl $ downUp keyLeftAlt

modeXTerm :: ModeM Commander ()
modeXTerm = mode "xterm" $ do
  command "+N1"               $ downUp keyQ
  fastMouseWheel

{- ########################################################################################## -}

switcher :: (ByteString -> IO ()) -> WinM ()
switcher set = do
  mClass1 "Firefox"     $ set "firefox"
  mClass0 "gedit"       $ set "gedit"
  mClass0 "krusader"    $ set "krusader"
  mClass1 "MPlayer"     $ set "mplayer"
  mClass0 "opera"       $ set "opera"  -- CHECK THIS
  mClass0 "sublime"     $ set "sublime"
  mClass0 "vmware"      $ set "vmware"
  mClass0 "xterm"       $ set "xterm"
  mName "Event Tester"  $ set "xterm"
  mAny                  $ set "default"

{- ########################################################################################## -}

-- toggle suspend mode

main :: IO ()
main = daemon $ do
  lift $ putStrLn "[*] Starting ..."
  evdev       <- mkEVDevSourceDyn mdev
  uinput0     <- mkUInputSink "UInput: Primary"
  uinput1     <- mkUInputSink "UInput: DisplayLink"
  macro       <- newMacroFilter
  debug       <- mkDebugFilter
  
  (cmd, hub)  <- newCommander sesId $ do
    addSink "Main"  uinput0
    addSink "Dl"    uinput1
    setModeSwitcher macro switcher
  
  runMode macro cmd $ do
    registerKeys
    command "*FN+Compose" $ nextProfile cmd

    mode "global" $ do
      mode "apps"         $ modeApps
      mode "guayadeque"   $ modeGuayadeque
      mode "xmonad"       $ modeXMonad

    mode "local" $ do
      modeDefault
      modeFirefox
      modeGEdit
      modeKrusader
      modeMPlayer
      modeOpera
      modeSublime
      modeVMware
      modeXTerm
  
  evdev >>> macro >>> hub
  lift $ putStrLn "[*] Initialized ..."
  lift $ forever $ threadDelay 500000000

{- ########################################################################################## -}
