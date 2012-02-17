{-# LANGUAGE OverloadedStrings #-}

import            Control.Concurrent (threadDelay)
import            Control.Monad (forever, forM_, when)
import qualified  Data.ByteString.Char8 as B
import            F.CommandD.Daemon
import            F.CommandD.Action.Eclipse
import            F.CommandD.Action.Firefox
import            F.CommandD.Action.Guayadeque
import            F.CommandD.Action.Macro
import            F.CommandD.Action.Mode
import            F.CommandD.Action.Run
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
  * manual mode override with suspended switcher / toggle
  * run outside session
  * commander: handle session destruction

  * evdev hotplug/reload
  * TCP source/sink

-}

{- ########################################################################################## -}

r :: String -> String
r = id

{- ########################################################################################## -}

-- Select input devices
mdev :: Word16 -> Word16 -> IO SourceId
mdev 0x1532 0x010d  = return 0      -- BlackWidow
mdev 0x1532 0x001f  = return 1      -- Naga
mdev _      _       = return (-1)

-- Filter session processes
sesFilt :: ByteString -> Bool
sesFilt "dwm"             = True
sesFilt "fmonad"          = True
sesFilt "xfce4-session"   = True
sesFilt _                 = False

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
  aliasDev 0 "B" -- BlackWindow
  aliasDev 1 "N" -- Naga
  aliasKey "LeftMeta" "B" "Super"
  aliasKey "CapsLock" "B" "Hyper"
  aliasKey "F24"      "B" "FN"
  aliasKey "@Left"    "N" "ML"
  aliasKey "@Right"   "N" "MR"
  forM_ (zip bwidKeys0 bwidKeys1) $ \(k0, k1) -> aliasKey k0 "B" (B.pack k1)
  forM_ (zip nagaKeys0 nagaKeys1) $ \(k0, k1) -> aliasKey k0 "N" (B.pack k1)
  
{- ########################################################################################## -}
  
-- mode to launch applications
modeApps :: ModeM Commander ()
modeApps = mode "apps" $ do
  command "*LeftAlt+F2"       $ r "`dmenu_path | dmenu -b`"
  command "!Super+B"          $ r "VirtualBox"
  command "!Super+C"          $ r "urxvt -e python"
  command "!Super+F!Super+F"  $ r "firefox"
  command "!Super+F!Super+M"  $ r "Thunar"
  command "!Super+G"          $ r "guayadeque"
  command "!Super+H"          $ r "krusader --left ~ --right ~"
  command "!Super+O"          $ r "opera"
  command "!Super+R"          $ r "transmission-gtk"
  command "!Super+S"          $ r "skype"
  command "!Super+T"          $ r "urxvt -e tmux"
  command "!Super+Y"          $ r "pkexec synaptic"
  command "!Super+V"          $ r "vmware"
  command "!Super+X!Super+P"  $ r "xterm -e 'xprop && read'"
  command "!Super+X!Super+X"  $ r "xterm -e '/home/angel/code/app/FCommandD/install.sh'"
  command "!Super+Z"          $ at "Dl" $ run "zim" [] >> activate

-- mode to control Guayadeque via DBus
modeGuayadeque :: ModeM Commander ()
modeGuayadeque = mode "guayadeque" $ do
  command "+NextSong"         $ gdqNext
  command "+PlayPause"        $ gdqPlayPause
  command "+PreviousSong"     $ gdqPrev
  command "+StopCD"           $ gdqStop

  command "*Hyper+N10"        $ gdqPrev
  command "*Hyper+N11"        $ gdqPlayPause
  command "*Hyper+N12"        $ gdqNext

modeVolume :: ModeM Commander ()
modeVolume = mode "volume" $ do
  command "*Hyper+WheelDown:N"  $ downUp keyVolumeDown
  command "*Hyper+WheelUp:N"    $ downUp keyVolumeUp

-- mode to control XMonad via DBus
modeXMonad :: ModeM Commander ()
modeXMonad = mode "xmonad" $ do
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

fastMouseWheel :: Int32 -> ModeM Commander ()
fastMouseWheel speed = do
  command "+WheelDown:N"          $ rel relWheel $ -speed
  command "+WheelUp:N"            $ rel relWheel $  speed
  command "*LeftCtrl+WheelDown:N" $ rel relWheel $ -speed * 2
  command "*LeftCtrl+WheelUp:N"   $ rel relWheel $  speed * 2

modeDefault :: ModeM Commander ()
modeDefault = mode "default" $ do
  fastMouseWheel 5

modeEclipse :: ModeM Commander ()
modeEclipse = mode "eclipse" $ do
  fastMouseWheel 5
  command "!N1+ML"            $ eclipseGoBack
  command "!N1+MR"            $ eclipseGoForward
  command "!N2+ML"            $ eclipseOpenDeclaration
  command "!N2+MR"            $ eclipseFindReferences

modeFirefox :: ModeM Commander ()
modeFirefox = mode "firefox" $ do
  command "+N1"               $ firefoxGoBack
  command "+N2"               $ firefoxTabClose
  command "+N3"               $ firefoxGoForward
  command "+N4"               $ firefoxOpenInNewTab
  command "*LeftShift+N4"     $ firefoxSearchForSel

modeGEdit :: ModeM Commander ()
modeGEdit = mode "gedit" $ do
  fastMouseWheel 5

modeKrusader :: ModeM Commander ()
modeKrusader = mode "krusader" $ do
  return ()

modeLibreOfficeWriter :: ModeM Commander ()
modeLibreOfficeWriter = mode "lowriter" $ do
  command "+N1"               $ hold keyLeftCtrl $ downUp keyB

modeMPlayer :: ModeM Commander ()
modeMPlayer = mode "mplayer" $ do
  command "+WheelDown:N"      $ downUp keyVolumeDown
  command "+WheelUp:N"        $ downUp keyVolumeUp

modeOpera :: ModeM Commander ()
modeOpera = mode "opera" $ do
  return ()

modeSublime :: ModeM Commander ()
modeSublime = mode "sublime" $ do
  fastMouseWheel 5

modeVMware :: ModeM Commander ()
modeVMware = mode "vmware" $ do
  command "+Hyper" $ hold keyLeftCtrl $ downUp keyLeftAlt

modeXTerm :: ModeM Commander ()
modeXTerm = mode "xterm" $ do
  command "+N1"               $ downUp keyQ
  fastMouseWheel 5

{- ########################################################################################## -}

modeSesDl :: ModeM Commander ()
modeSesDl = mode "Dl" $ do
  command "!M1+Grave" $ do
    at "Dl"   $ xmonadWkSetCurrent "1"
    at "Main" $ activate

modeSesMain :: ModeM Commander ()
modeSesMain = mode "Main" $ do
  command "!M1+Grave" $ nextSession

{- ########################################################################################## -}

switcher = do
  mClass0 "Eclipse"             $ setModeLSX "eclipse"
  mClass1 "Firefox"             $ setModeLSX "firefox"
  mClass0 "gedit"               $ setModeLSX "gedit"
  mClass0 "krusader"            $ setModeLSX "krusader"
  mClass1 "libreoffice-writer"  $ setModeLSX "lowriter"
  mClass1 "MPlayer"             $ setModeLSX "mplayer"
  mClass0 "opera"               $ setModeLSX "opera"
  mClass0 "sublime"             $ setModeLSX "sublime"
  mClass0 "vmware"              $ setModeLSX "vmware"
  mClass0 "xterm"               $ setModeLSX "xterm"
  mName "Event Tester"          $ setModeLSX "xterm"
  mAny                          $ setModeLSX "default"

{- ########################################################################################## -}

main :: IO ()
main = daemon $ do
  lift $ putStrLn "[*] Starting ..."
  evdev       <- mkEVDevSourceDyn mdev
  uinput0     <- mkUInputSink "UInput: Primary"
  uinput1     <- mkUInputSink "UInput: DisplayLink"
  debug       <- mkDebugFilter
  
  (cmd, macro, hub)  <- newCommander sesId $ do
    addSink "Main"  uinput0 $ return () 
    addSink "Dl"    uinput1 $ return ()
    setFocusHook          $ modeSwitcher switcher
    setSessionFilter      $ sesFilt
    setSessionSwitchHook  $ enableSessionMode
  
  runMode macro cmd $ do
    registerKeys
    command "!M1+ESC"     $ nextSession
    command "*Hyper+F11"  $ run1 "gnome-calculator" ""
    command "*Hyper+F12"  $ toggleModes2 ["local"] ["test"] 

    mode "global" $ do
      modeApps
      modeGuayadeque
      modeVolume
      modeXMonad

    mode "local" $ do
      modeDefault
      modeEclipse
      modeFirefox
      modeGEdit
      modeKrusader
      modeLibreOfficeWriter
      modeMPlayer
      modeOpera
      modeSublime
      modeVMware
      modeXTerm

    mode "session" $ do
      modeSesMain
      modeSesDl

    mode0 "test" $ do
      command "+N1" $ xmonadWkSetCurrentG "0_1"
  
  evdev >>> macro >>> hub
  initCommander cmd

  lift $ putStrLn "[*] Initialized ..."
  lift $ forever $ threadDelay 50000000
  -- lift $ threadDelay 5000000

{- ########################################################################################## -}
