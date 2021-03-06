{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import            Control.Concurrent (threadDelay)
import            Control.Monad (forever, forM_, replicateM_, when)
import qualified  Data.ByteString.Char8 as B
import            F.CommandD.Daemon
import qualified  F.CommandD.Device.CyborgMMO7 as C
import            F.CommandD.Action.Eclipse
import            F.CommandD.Action.Firefox
import            F.CommandD.Action.Guayadeque
import            F.CommandD.Action.I3 
import            F.CommandD.Action.Macro
import            F.CommandD.Action.Mode
import            F.CommandD.Action.NetBeans
import            F.CommandD.Action.RubyMine
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
  * run outside session
  * commander: handle session destruction

  * evdev hotplug/reload
  * TCP source/sink
  * dynamic nodes? vim like stuff

  TODO:
  * persistenly pressed keys
  * held key
  * double/tripple clicks

-}

{- ########################################################################################## -}

r :: String -> String
r = id

{- ########################################################################################## -}

-- Select input devices
mdev :: Word16 -> Word16 -> IO SourceId
mdev 0x1532 0x010d  = return 0      -- BlackWidow
mdev 0x1532 0x001f  = return 1      -- Naga
mdev 0x1532 0x0021  = return 1      -- Naga
mdev 0x06A3 0x0CD0  = return 1      -- Cyborg
mdev _      _       = return (-1)

-- Filter session processes
sesFilt :: ByteString -> Bool
sesFilt s = any (== s) 
  [ "dwm"
  , "i3"
  , "/usr/bin/i3"
  , "fmonad"
  , "/home/angel/.cabal/bin/fmonad"
  , "lxpanel"
  ]

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
  forM_ (zip bwidKeys0 bwidKeys1)     $ \(k0, k1) -> aliasKey  k0 "B" (B.pack k1)
  forM_ (zip nagaKeys0 nagaKeys1)     $ \(k0, k1) -> aliasKey  k0 "N" (B.pack k1)
  forM_ C.keys $ \(k0, k1) -> aliasKey' k0 "N" (B.pack $ 'C':k1)
  
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
  command "!Super+T"          $ r "xterm" --  "urxvt -e tmux"
  command "!Super+Y"          $ r "pkexec synaptic"
  command "!Super+V"          $ r "vmware"
  command "!Super+X!Super+P"  $ r "xterm -e 'xprop && read'"
  command "!Super+X!Super+X"  $ r "xterm -e '/home/angel/code/app/FCommandD/install.sh'"
  command "!Super+Z"          $ at "Dl" $ run "zim" [] >> activate

  command "!Super+PageDown"   $ r "lxsession-logout"

-- mode to control Guayadeque via DBus
modeGuayadeque :: ModeM Commander ()
modeGuayadeque = mode "guayadeque" $ do
  command "*FN+NextSong"      $ gdqNext
  command "*FN+PlayPause"     $ gdqPlayPause
  command "*FN+PreviousSong"  $ gdqPrev
  command "*FN+StopCD"        $ gdqStop

  command "*CS+CWest"         $ gdqPrev
  command "*CS+CCenter"       $ gdqPlayPause
  command "*CS+CEast"         $ gdqNext

  command "*Hyper+N10"        $ gdqPrev
  command "*Hyper+N11"        $ gdqPlayPause
  command "*Hyper+N12"        $ gdqNext

modeVolume :: ModeM Commander ()
modeVolume = mode "volume" $ do
  command "*Hyper+WheelDown:N"  $ downUp keyVolumeDown
  command "*Hyper+WheelUp:N"    $ downUp keyVolumeUp

modeI3 :: ModeM Commander ()
modeI3 = mode "i3" $ do
  -- Directional window navigation
  forM_ (zip "ADWS" ["left", "right", "up", "down"]) $ \(key, dir) -> do
    let c mod pref = command (mod ++ [key]) $ i3Cmd $ concat [pref, " ", dir]
    c "*LeftAlt+"               "focus"
    c "*LeftAlt*LeftShift+"     "move"

  -- Layouts
  command "*LeftAlt+1"    $ i3Cmd "split h"
  command "*LeftAlt+2"    $ i3Cmd "split v"
  command "*LeftAlt+Q"    $ i3Cmd "focus parent"
  command "*LeftAlt+E"    $ i3Cmd "focus child"
  command "*LeftAlt+Z"    $ i3Mode "resize"

  command "*Hyper+F1"     $ i3Cmd "layout stacking"
  command "*Hyper+F2"     $ i3Cmd "layout tabbed"
  command "*Hyper+F3"     $ i3Cmd "layout default"
  command "*Hyper+C"      $ i3Cmd "kill"
  command "*Hyper+Space"  $ i3Cmd "floating toggle"

  command "+CWest"        $ i3SetWk "A4"
  command "+CSouth"       $ i3SetWk "A6"
  command "+CEast"        $ i3SetWk "B3"

  -- Workspaces
  let wk = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8"
           ,"B1", "B2", "B3", "B4", "B5", "B6"]
  forM_ (zip "12QWASZX34ERDF" wk) $ \(key, wk) -> do
    command ("*Hyper+"         ++ [key]) $ i3SetWk  wk
    command ("*Hyper*LeftAlt+" ++ [key]) $ i3MoveWk wk

-- mode to control XMonad via DBus
modeXMonad :: ModeM Commander ()
modeXMonad = mode "xmonad" $ do
  command "*Hyper+F9"         $ xmonadCoreExit
  command "*Hyper+F10"        $ xmonadCoreRestart
  command "*Hyper+K"          $ xmonadLayoutExpand
  command "*Hyper+Backslash"  $ xmonadLayoutNext
  command "*Hyper+J"          $ xmonadLayoutShrink
  command "*Hyper+Space"      $ xmonadMasterFocus
  command "*Hyper+Minus"      $ xmonadMasterMod (-1)
  command "*Hyper+Equal"      $ xmonadMasterMod ( 1)
  command "*Hyper+Enter"      $ xmonadMasterSwap
  command "*Hyper+Grave"      $ xmonadNavGridSelect
  command "*LeftAlt+Q"        $ xmonadTabPrev
  command "*LeftAlt+E"        $ xmonadTabNext
  command "*Hyper+U"          $ xmonadTabUnmerge
  command "*Hyper+C"          $ xmonadWinClose
  command "*Hyper+T"          $ xmonadWinSink

  -- Workspace navigation
  let wk = [(0, "A0"), (0, "A1"), (0, "A2"), (0, "A3")
           ,(0, "A4"), (0, "A5"), (0, "A6"), (0, "A7")
           ,(1, "B0"), (1, "B1"), (1, "B2"), (1, "B3")
           ,(1, "B4"), (1, "B5")
           ]

  forM_ (zip "12QWASZX34ERDF" wk) $ \(key, (sc, wk)) -> do
    command ("*Hyper+"         ++ [key]) $ xmonadScreenSetWk sc wk
    command ("*Hyper*LeftAlt+" ++ [key]) $ xmonadWinMoveTo wk

  -- Directional window navigation
  forM_ (zip ["A", "D", "W", "S"] [0, 1, 2, 3]) $ \(key, dir) -> do
    command ("*LeftAlt+"            ++ key) $ xmonadNavGo dir
    command ("*LeftAlt*LeftShift+"  ++ key) $ xmonadNavSwap dir
    command ("*LeftAlt*RightAlt+"   ++ key) $ xmonadTabMerge dir

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
  
  command "!M4"              $ eclipseGoBack
  command "!M5"              $ eclipseGoForward

modeFirefox :: ModeM Commander ()
modeFirefox = mode "firefox" $ do
  command "+N1"               $ firefoxGoBack
  command "+N2"               $ firefoxTabClose
  command "+N3"               $ firefoxGoForward
  command "+N4"               $ firefoxOpenInNewTab
  command "*LeftShift+N4"     $ firefoxSearchForSel

  command "+CHLeft"           $ firefoxGoBack
  command "+CThumb"           $ firefoxTabClose
  command "+CHRight"          $ firefoxGoForward

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

modeNetBeans :: ModeM Commander ()
modeNetBeans = mode "netbeans" $ do
  command "!M1"               $ nbQuickOpenFile

modeOpera :: ModeM Commander ()
modeOpera = mode "opera" $ do
  return ()

modeRubyMine :: ModeM Commander ()
modeRubyMine = mode "rubymine" $ do
  command "+N1"               $ rubyMineRun
  command "+CThumb"           $ rubyMineRun

modeSublime :: ModeM Commander ()
modeSublime = mode "sublime" $ do
  fastMouseWheel 5

modeVMware :: ModeM Commander ()
modeVMware = mode "vmware" $ do
  command "+Hyper" $ hold keyLeftCtrl $ downUp keyLeftAlt

modeXTerm :: ModeM Commander ()
modeXTerm = mode "xterm" $ do
  -- command "+N1"               $ downUp keyQ
  fastMouseWheel 5

{- ########################################################################################## -}

-- a mode for utility session
modeSesDl :: ModeM Commander ()
modeSesDl = mode "Dl" $ do
  command "!M1+Grave" $ do
    at "Dl"   $ xmonadWkSetCurrent "A0"
    at "Main" $ activate

-- a mode to switch between sessions
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
  mClass1 "netbeans"            $ setModeLSX "netbeans" -- TODO: fix class
  mClass0 "opera"               $ setModeLSX "opera"
  mClass1 "jetbrains-rubymine"  $ setModeLSX "rubymine"
  mClass0 "sublime"             $ setModeLSX "sublime"
  mClass0 "vmware"              $ setModeLSX "vmware"
  mClass0 "xterm"               $ setModeLSX "xterm"
  mName "Event Tester"          $ setModeLSX "xterm"
  mAny                          $ setModeLSX "default"

{- ########################################################################################## -}

main :: IO ()
main = daemon $ do
  lift $ putStrLn "[*] Starting 2 ..."
  evdev       <- mkEVDevSourceDyn mdev
  uinput0     <- mkUInputSink "UInput: Primary"
  uinput1     <- mkUInputSink "UInput: DisplayLink"
  debug       <- mkDebugFilter
  
  (cmd, macro, hub)  <- newCommander sesFilt sesId $ do
    addSink "Main"  uinput0 $ return () 
    addSink "Dl"    uinput1 $ return ()
    setFocusHook          $ modeSwitcher switcher
    setSessionSwitchHook  $ enableSessionMode
  
  runMode macro cmd $ do
    registerKeys

    command "+CU1" $ do
      lift $ threadDelay 100000 
      forM_ [keyC, keyO, keyO, keyK, keyI, keyE] $ \k -> do
        downUp k
        lift $ threadDelay 100000 

    command "*Hyper+P" $ do
      lift $ threadDelay 1000000 
      replicateM_ 5 $ do
        forM_ [keyC, keyO, keyO, keyK, keyI, keyE, key2] $ \k -> do
          hold keyLeftShift $ downUp k
          lift $ threadDelay 200000 
          downUp keyDown

    command "!CRed+F14"   $ putStrLn "RED"
    command "!CBlue+F14"  $ putStrLn "BLUE"
    command "!CPink+F14"  $ putStrLn "PINK"

    {-command "+M1"         $ putStrLn "X"
    command "+M2"         $ downUp keyB
    command "+M3"         $ downUp keyC
    command "+M4"         $ downUp keyD -}
    command "+M5"         $ downUp keyE 

    command "!M1+ESC"     $ nextSession
    command "*Hyper+F9"   $ toggleModes [["global", "xmonad"]]
    command "*Hyper+F10"  $ toggleModes [["global", "apps"]]
    command "*Hyper+F11"  $ run1 "gnome-calculator" ""
    command "*Hyper+F12"  $ toggleModes [["local"], ["test"]] 

    mode "global" $ do
      modeApps
      modeGuayadeque
      modeVolume
      --modeI3
      modeXMonad

    mode "local" $ do
      modeDefault
      modeEclipse
      modeFirefox
      modeGEdit
      modeKrusader
      modeLibreOfficeWriter
      modeMPlayer
      modeNetBeans
      modeOpera
      modeRubyMine
      modeSublime
      modeVMware
      modeXTerm

    {-mode "session" $ do
      modeSesMain
      modeSesDl -}

    mode0 "test" $ do
      command "+N1" $ xmonadWkSetCurrentG "0_1"
  
  -- debug >>> 
  evdev >>> macro >>> hub
  initCommander cmd

  lift $ putStrLn "[*] Initialized ..."
  lift $ forever $ threadDelay 50000000
  -- lift $ threadDelay 5000000

{- ########################################################################################## -}
