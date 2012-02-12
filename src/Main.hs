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
bwidKeys0 = ["F13", "F14", "F15", "F16", "F17"]
bwidKeys1 = ["M1" , "M2" , "M3" , "M4" , "M5" ]
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
  aliasKey "F24"      "B" "FN"
  forM_ (zip bwidKeys0 bwidKeys1) $ \(k0, k1) -> aliasKey k0 "B" (B.pack k1)
  forM_ (zip nagaKeys0 nagaKeys1) $ \(k0, k1) -> aliasKey k0 "N" (B.pack k1)
  
{- ########################################################################################## -}
  
-- mode to launch applications
modeApps :: Actions -> ModeM ()
modeApps (A _ _ r) = do
  command "*LeftAlt+F2"       $ r "`dmenu_path | dmenu -b`"     []
  command "*Super+B"          $ r "VirtualBox"                  []
  command "*Super+C"          $ r "urxvt -e python"             []
  command "*Super+F*Super+F"  $ r "firefox"                     []
  command "*Super+F*Super+M"  $ r "Thunar"                      []
  command "*Super+H"          $ r "krusader --left ~ --right ~" []
  command "*Super+O"          $ r "opera"                       []
  command "*Super+R"          $ r "transmission-gtk"            []
  command "*Super+S"          $ r "skype"                       []
  command "*Super+T"          $ r "urxvt -e tmux"               []
  command "*Super+Y"          $ r "pkexec synaptic"             []
  command "*Super+V"          $ r "vmware"                      []

-- mode to control Guayadeque via DBus
modeGuayadeque :: Actions -> ModeM ()
modeGuayadeque (A c _ _) = do
  command "+NextSong"         $ c $ gdqNext
  command "+PlayPause"        $ c $ gdqPlayPause
  command "+PreviousSong"     $ c $ gdqPrev
  command "+StopCD"           $ c $ gdqStop

  command "*Hyper+N10"        $ c $ gdqPrev
  command "*Hyper+N11"        $ c $ gdqPlayPause
  command "*Hyper+N12"        $ c $ gdqNext

-- mode to control XMonad via DBus
modeXMonad :: Actions-> ModeM ()
modeXMonad (A c _ _) = do
  command "*Hyper+Z"          $ c $ xmonadCoreExit
  command "*Hyper+X"          $ c $ xmonadCoreRestart
  command "*Hyper+H"          $ c $ xmonadCoreSetWMName "L3GD"
  command "*Hyper+K"          $ c $ xmonadLayoutExpand
  command "*Hyper+Backslash"  $ c $ xmonadLayoutNext
  command "*Hyper+J"          $ c $ xmonadLayoutShrink
  command "*Hyper+Space"      $ c $ xmonadMasterFocus
  command "*Hyper+Minus"      $ c $ xmonadMasterMod (-1)
  command "*Hyper+Equal"      $ c $ xmonadMasterMod ( 1)
  command "*Hyper+Enter"      $ c $ xmonadMasterSwap
  command "*Hyper+Grave"      $ c $ xmonadNavGridSelect
  command "*Hyper+RightBrace" $ c $ xmonadTabNext
  command "*Hyper+LeftBrace"  $ c $ xmonadTabPrev
  command "*Hyper+U"          $ c $ xmonadTabUnmerge
  command "*Hyper+C"          $ c $ xmonadWinClose
  command "*Hyper+T"          $ c $ xmonadWinSink

  -- command "+N1"               $ call $ xmonadWkSetCurrent "1"

  -- Workspace navigation
  forM_ (zip "1234QWERASDF" "123456789ABC") $ \(key, wk) -> do
    command ("*Hyper+"         ++ [key]) $ c $ xmonadWkSetCurrent [wk]
    command ("*Hyper*LeftAlt+" ++ [key]) $ c $ xmonadWkMoveWindow [wk]

  -- Screen navigation
  forM_ (zip3 ["Comma", "Dot"] ["N4", "N6"] [0, 1]) $ \(k0, k1, scr) -> do
    command ("*Hyper+"         ++ k0) $ c $ xmonadScreenSetCurr scr
    command ("*Hyper*LeftAlt+" ++ k0) $ c $ xmonadScreenMoveWin scr
    command ("*Hyper+"         ++ k1) $ c $ xmonadScreenMoveWin scr

  -- Directional window navigation
  forM_ (zip ["L", "Apostrophe", "P", "SemiColon"] [0, 1, 2, 3]) $ \(key, dir) -> do
    command ("*Hyper+"         ++ key) $ c $ xmonadNavMove dir
    command ("*Hyper*LeftAlt+" ++ key) $ c $ xmonadTabMerge dir

{- ########################################################################################## -}

fastMouseWheel :: Actions -> ModeM ()
fastMouseWheel (A _ m _) = do
  command "+WheelDown:N"      $ m $ rel relWheel (-5)
  command "+WheelUp:N"        $ m $ rel relWheel ( 5)

modeDefault :: Actions -> ModeM ()
modeDefault a@(A _ _ _) = mode "default" $ do
  fastMouseWheel a

modeFirefox :: Actions -> ModeM ()
modeFirefox (A _ m _) = mode "firefox" $ do
  command "+N1"               $ m $ firefoxGoBack
  command "+N2"               $ m $ firefoxTabClose
  command "+N3"               $ m $ firefoxGoForward
  command "+N4"               $ m $ firefoxOpenInNewTab
  command "*LeftShift+N4"     $ m $ firefoxSearchForSel

modeGEdit :: Actions -> ModeM ()
modeGEdit a@(A _ _ _) = mode "gedit" $ do
  fastMouseWheel a

modeKrusader :: Actions -> ModeM ()
modeKrusader (A _ _ _) = mode "krusader" $ do
  return ()

modeMPlayer :: Actions -> ModeM ()
modeMPlayer (A _ _ _) = mode "mplayer" $ do
  return ()

modeOpera :: Actions -> ModeM ()
modeOpera (A _ _ _) = mode "opera" $ do
  return ()

modeSublime :: Actions -> ModeM ()
modeSublime a@(A _ m _) = mode "sublime" $ do
  fastMouseWheel a

modeVMware :: Actions -> ModeM ()
modeVMware (A _ m _) = mode "vmware" $ do
  command "+Hyper" $ m $ hold keyLeftCtrl $ downUp keyLeftAlt

modeXTerm :: Actions -> ModeM ()
modeXTerm a@(A _ m _) = mode "xterm" $ do
  command "+N1"               $ m $ downUp keyQ
  fastMouseWheel a

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
  a           <- getActions cmd hub
  
  runMode macro $ do
    registerKeys
    command "*FN+Compose" $ nextProfile cmd

    mode "apps"         $ modeApps a
    mode "guayadeque"   $ modeGuayadeque a
    mode "xmonad"       $ modeXMonad a

    mode "local" $ do
      modeDefault   a
      modeFirefox   a
      modeGEdit     a
      modeKrusader  a
      modeMPlayer   a
      modeOpera     a
      modeSublime   a
      modeVMware    a
      modeXTerm     a
  
  evdev >>> macro >>> hub
  lift $ putStrLn "[*] Initialized ..."
  lift $ forever $ threadDelay 500000000

{- ########################################################################################## -}
