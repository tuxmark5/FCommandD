{-# LANGUAGE OverloadedStrings #-}

module F.CommandD.Util.KeyMap
( Key(..)
, KeyMap(..)
, addDev
, addKey
, defaultDevices
, defaultKey
, defaultKeyMap
, defaultKeys
, lookupDev
, lookupDevKey
, lookupDevKey'
, lookupKey
, mkKeyMap
) where
  
{- ########################################################################################## -}
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as B
import            Data.Int (Int32)
import            Data.Map (Map)
import qualified  Data.Map as M
import            Data.Word (Word16)
import            System.Linux.Keys
{- ########################################################################################## -}

data Key  = Key
  { keyCode       :: !Word16
  , keyDevice     :: !Int32
  , keyFlag       :: !Int32
  } 
  
data KeyMap = KeyMap
  { keyMapDevices :: Map ByteString Int32
  , keyMapKeys    :: Map ByteString Key
  }
  
instance Eq Key where
  a == b = keyCode a == keyCode b && keyDevice a == keyDevice b
  
instance Ord Key where
  compare a b | keyCode a == keyCode b  = compare (keyDevice a) (keyDevice b)
              | keyCode a <  keyCode b  = LT
              | otherwise               = GT

instance Show Key where
  show key = show $ keyCode key
              
{- ########################################################################################## -}

addDev :: KeyMap -> ByteString -> Int32 -> KeyMap
addDev kmap name dev = kmap
  { keyMapDevices = M.insert name dev (keyMapDevices kmap) }  
    
addKey :: KeyMap -> ByteString -> Int32 -> Word16 -> KeyMap
addKey kmap name dev key = kmap
  { keyMapKeys = M.insert name k (keyMapKeys kmap) } where 
  k = Key
    { keyCode     = key
    , keyDevice   = dev
    , keyFlag     = 0
    }

defaultKey :: Key
defaultKey = Key
  { keyCode     = 0
  , keyDevice   = 0
  , keyFlag     = 0
  }

defaultKeyMap :: KeyMap
defaultKeyMap = mkKeyMap defaultDevices defaultKeys

lookupDev :: KeyMap -> ByteString -> Maybe Int32
lookupDev kmap dev = M.lookup dev $ keyMapDevices kmap

lookupDevKey :: KeyMap -> ByteString -> ByteString -> Maybe Key
lookupDevKey kmap dev key = let
  d0 = M.lookup dev $ keyMapDevices kmap
  k0 = M.lookup key $ keyMapKeys    kmap
  in case (d0, k0) of
    (Just 0,   Just k) -> k0
    (Just d,   Just k) -> Just $ k { keyDevice = d }
    (_,       _      ) -> Nothing

lookupDevKey' :: KeyMap -> String -> String -> Maybe Key
lookupDevKey' kmap dev key = lookupDevKey kmap (B.pack dev) (B.pack key)

lookupKey :: KeyMap -> ByteString -> Maybe Key
lookupKey kmap key = M.lookup key $ keyMapKeys kmap

mkKeyMap :: [(ByteString, Int32)] -> [(ByteString, Word16)] -> KeyMap
mkKeyMap devs keys = KeyMap
  { keyMapDevices = M.fromList devs
  , keyMapKeys    = M.fromList $ map (\(n, i) -> (n, Key i 0 0)) keys
  }
  
{- ########################################################################################## -}

defaultDevices :: [(ByteString, Int32)]
defaultDevices = 
  [ ("",                    0                   )
  , ("all",                 0                   )
  ]

{- ########################################################################################## -}

defaultKeys :: [(ByteString, Word16)]
defaultKeys =
  [ ("@0",                  btn0                )
  , ("@1",                  btn1                )
  , ("@2",                  btn2                )
  , ("@3",                  btn3                )
  , ("@4",                  btn4                )
  , ("@5",                  btn5                )
  , ("@6",                  btn6                )
  , ("@7",                  btn7                )
  , ("@8",                  btn8                )
  , ("@9",                  btn9                )
  , ("@A",                  btnA                )
  , ("@B",                  btnB                )
  , ("@Back",               btnBack             )
  , ("@Base",               btnBase             )
  , ("@Base2",              btnBase2            )
  , ("@Base3",              btnBase3            )
  , ("@Base4",              btnBase4            )
  , ("@Base5",              btnBase5            )
  , ("@Base6",              btnBase6            )
  , ("@C",                  btnC                )
  , ("@Dead",               btnDead             )
  , ("@Digi",               btnDigi             )
  , ("@Extra",              btnExtra            )
  , ("@Forward",            btnForward          )
  , ("@GamePad",            btnGamePad          )
  , ("@GearDown",           btnGearDown         )
  , ("@GearUp",             btnGearUp           )
  , ("@Joystick",           btnJoystick         )
  , ("@Left",               btnLeft             )
  , ("@Middle",             btnMiddle           )
  , ("@Misc",               btnMisc             )
  , ("@Mode",               btnMode             )
  , ("@Mouse",              btnMouse            )
  , ("@Pinkie",             btnPinkie           )
  , ("@Right",              btnRight            )
  , ("@Select",             btnSelect           )
  , ("@Side",               btnSide             )
  , ("@Start",              btnStart            )
  , ("@Stylus",             btnStylus           )
  , ("@Stylus2",            btnStylus2          )
  , ("@Task",               btnTask             )
  , ("@Thumb",              btnThumb            )
  , ("@Thumb2",             btnThumb2           )
  , ("@ThumbL",             btnThumbL           )
  , ("@ThumbR",             btnThumbR           )
  , ("@TL",                 btnTL               )
  , ("@TL2",                btnTL2              )
  , ("@ToolAirbrush",       btnToolAirbrush     )
  , ("@ToolBrush",          btnToolBrush        )
  , ("@ToolDoubleTap",      btnToolDoubleTap    )
  , ("@ToolFinger",         btnToolFinger       )
  , ("@ToolLens",           btnToolLens         )
  , ("@ToolMouse",          btnToolMouse        )
  , ("@ToolPen",            btnToolPen          )
  , ("@ToolPencil",         btnToolPencil       )
  , ("@ToolQuadTap",        btnToolQuadTap      )
  , ("@ToolRubber",         btnToolRubber       )
  , ("@ToolTripleTap",      btnToolTripleTap    )
  , ("@Top",                btnTop              )
  , ("@Top2",               btnTop2             )
  , ("@Touch",              btnTouch            )
  , ("@TR",                 btnTR               )
  , ("@TR2",                btnTR2              )
  , ("@Trigger",            btnTrigger          )
  , ("@TriggerHappy",       btnTriggerHappy     )
  , ("@TriggerHappy1",      btnTriggerHappy1    )
  , ("@TriggerHappy10",     btnTriggerHappy10   )
  , ("@TriggerHappy11",     btnTriggerHappy11   )
  , ("@TriggerHappy12",     btnTriggerHappy12   )
  , ("@TriggerHappy13",     btnTriggerHappy13   )
  , ("@TriggerHappy14",     btnTriggerHappy14   )
  , ("@TriggerHappy15",     btnTriggerHappy15   )
  , ("@TriggerHappy16",     btnTriggerHappy16   )
  , ("@TriggerHappy17",     btnTriggerHappy17   )
  , ("@TriggerHappy18",     btnTriggerHappy18   )
  , ("@TriggerHappy19",     btnTriggerHappy19   )
  , ("@TriggerHappy2",      btnTriggerHappy2    )
  , ("@TriggerHappy20",     btnTriggerHappy20   )
  , ("@TriggerHappy21",     btnTriggerHappy21   )
  , ("@TriggerHappy22",     btnTriggerHappy22   )
  , ("@TriggerHappy23",     btnTriggerHappy23   )
  , ("@TriggerHappy24",     btnTriggerHappy24   )
  , ("@TriggerHappy25",     btnTriggerHappy25   )
  , ("@TriggerHappy26",     btnTriggerHappy26   )
  , ("@TriggerHappy27",     btnTriggerHappy27   )
  , ("@TriggerHappy28",     btnTriggerHappy28   )
  , ("@TriggerHappy29",     btnTriggerHappy29   )
  , ("@TriggerHappy3",      btnTriggerHappy3    )
  , ("@TriggerHappy30",     btnTriggerHappy30   )
  , ("@TriggerHappy31",     btnTriggerHappy31   )
  , ("@TriggerHappy32",     btnTriggerHappy32   )
  , ("@TriggerHappy33",     btnTriggerHappy33   )
  , ("@TriggerHappy34",     btnTriggerHappy34   )
  , ("@TriggerHappy35",     btnTriggerHappy35   )
  , ("@TriggerHappy36",     btnTriggerHappy36   )
  , ("@TriggerHappy37",     btnTriggerHappy37   )
  , ("@TriggerHappy38",     btnTriggerHappy38   )
  , ("@TriggerHappy39",     btnTriggerHappy39   )
  , ("@TriggerHappy4",      btnTriggerHappy4    )
  , ("@TriggerHappy40",     btnTriggerHappy40   )
  , ("@TriggerHappy5",      btnTriggerHappy5    )
  , ("@TriggerHappy6",      btnTriggerHappy6    )
  , ("@TriggerHappy7",      btnTriggerHappy7    )
  , ("@TriggerHappy8",      btnTriggerHappy8    )
  , ("@TriggerHappy9",      btnTriggerHappy9    )
  , ("@Wheel",              btnWheel            )
  , ("@X",                  btnX                )
  , ("@Y",                  btnY                )
  , ("@Z",                  btnZ                )
  , ("0",                   key0                )
  , ("1",                   key1                )
  , ("102nd",               key102nd            )
  , ("10ChannelsDown",      key10ChannelsDown   )
  , ("10ChannelsUp",        key10ChannelsUp     )
  , ("2",                   key2                )
  , ("3",                   key3                )
  , ("4",                   key4                )
  , ("5",                   key5                )
  , ("6",                   key6                )
  , ("7",                   key7                )
  , ("8",                   key8                )
  , ("9",                   key9                )
  , ("A",                   keyA                )
  , ("AB",                  keyAB               )
  , ("AddressBook",         keyAddressBook      )
  , ("Again",               keyAgain            )
  , ("Alterase",            keyAlterase         )
  , ("Angle",               keyAngle            )
  , ("Apostrophe",          keyApostrophe       )
  , ("Archive",             keyArchive          )
  , ("Audio",               keyAudio            )
  , ("Aux",                 keyAux              )
  , ("B",                   keyB                )
  , ("Back",                keyBack             )
  , ("Backslash",           keyBackslash        )
  , ("Backspace",           keyBackspace        )
  , ("BassBoost",           keyBassBoost        )
  , ("Battery",             keyBattery          )
  , ("Blue",                keyBlue             )
  , ("Bluetooth",           keyBluetooth        )
  , ("BookMarks",           keyBookMarks        )
  , ("Break",               keyBreak            )
  , ("BrightnessDown",      keyBrightnessDown   )
  , ("BrightnessUp",        keyBrightnessUp     )
  , ("BrightnessCycle",     keyBrightnessCycle  )
  , ("BrightnessZero",      keyBrightnessZero   )
  , ("BrlDot1",             keyBrlDot1          )
  , ("BrlDot10",            keyBrlDot10         )
  , ("BrlDot2",             keyBrlDot2          )
  , ("BrlDot3",             keyBrlDot3          )
  , ("BrlDot4",             keyBrlDot4          )
  , ("BrlDot5",             keyBrlDot5          )
  , ("BrlDot6",             keyBrlDot6          )
  , ("BrlDot7",             keyBrlDot7          )
  , ("BrlDot8",             keyBrlDot8          )
  , ("BrlDot9",             keyBrlDot9          )
  , ("C",                   keyC                )
  , ("Calc",                keyCalc             )
  , ("Calendar",            keyCalendar         )
  , ("Camera",              keyCamera           )
  , ("CameraDown",          keyCameraDown       )
  , ("CameraFocus",         keyCameraFocus      )
  , ("CameraLeft",          keyCameraLeft       )
  , ("CameraRight",         keyCameraRight      )
  , ("CameraUp",            keyCameraUp         )
  , ("CameraZoomIn",        keyCameraZoomIn     )
  , ("CameraZoomOut",       keyCameraZoomOut    )
  , ("Cancel",              keyCancel           )
  , ("CapsLock",            keyCapsLock         )
  , ("CD",                  keyCD               )
  , ("Channel",             keyChannel          )
  , ("ChannelDown",         keyChannelDown      )
  , ("ChannelUp",           keyChannelUp        )
  , ("Chat",                keyChat             )
  , ("Clear",               keyClear            )
  , ("Close",               keyClose            )
  , ("CloseCD",             keyCloseCD          )
  , ("Coffee",              keyCoffee           )
  , ("Comma",               keyComma            )
  , ("Compose",             keyCompose          )
  , ("Computer",            keyComputer         )
  , ("Config",              keyConfig           )
  , ("Connect",             keyConnect          )
  , ("ContactMenu",         keyContactMenu      )
  , ("Copy",                keyCopy             )
  , ("Cut",                 keyCut              )
  , ("CycleWindows",        keyCycleWindows     )
  , ("D",                   keyD                )
  , ("Bashboard",           keyBashboard        )
  , ("Database",            keyDatabase         )
  , ("Delete",              keyDelete           )
  , ("DeleteFile",          keyDeleteFile       )
  , ("DelEOL",              keyDelEOL           )
  , ("DelEOS",              keyDelEOS           )
  , ("DelLine",             keyDelLine          )
  , ("Digits",              keyDigits           )
  , ("Direction",           keyDirection        )
  , ("Directory",           keyDirectory        )
  , ("DisplayToggle",       keyDisplayToggle    )
  , ("DisplayOff",          keyDisplayOff       )
  , ("Documents",           keyDocuments        )
  , ("Dollar",              keyDollar           )
  , ("Dot",                 keyDot              )
  , ("Down",                keyDown             )
  , ("DVD",                 keyDVD              )
  , ("E",                   keyE                )
  , ("Edit",                keyEdit             )
  , ("Editor",              keyEditor           )
  , ("EjectCT",             keyEjectCT          )
  , ("EjectCloseCD",        keyEjectCloseCD     )
  , ("Email",               keyEmail            )
  , ("End",                 keyEnd              )
  , ("Enter",               keyEnter            )
  , ("EPG",                 keyEPG              )
  , ("Equal",               keyEqual            )
  , ("ESC",                 keyESC              )
  , ("Euro",                keyEuro             )
  , ("Exit",                keyExit             )
  , ("F",                   keyF                )
  , ("F1",                  keyF1               )
  , ("F10",                 keyF10              )
  , ("F11",                 keyF11              )
  , ("F12",                 keyF12              )
  , ("F13",                 keyF13              )
  , ("F14",                 keyF14              )
  , ("F15",                 keyF15              )
  , ("F16",                 keyF16              )
  , ("F17",                 keyF17              )
  , ("F18",                 keyF18              )
  , ("F19",                 keyF19              )
  , ("F2",                  keyF2               )
  , ("F20",                 keyF20              )
  , ("F21",                 keyF21              )
  , ("F22",                 keyF22              )
  , ("F23",                 keyF23              )
  , ("F24",                 keyF24              )
  , ("F3",                  keyF3               )
  , ("F4",                  keyF4               )
  , ("F5",                  keyF5               )
  , ("F6",                  keyF6               )
  , ("F7",                  keyF7               )
  , ("F8",                  keyF8               )
  , ("F9",                  keyF9               )
  , ("FastForward",         keyFastForward      )
  , ("Favorites",           keyFavorites        )
  , ("File",                keyFile             )
  , ("Finance",             keyFinance          )
  , ("Find",                keyFind             )
  , ("First",               keyFirst            )
  , ("Fn",                  keyFn               )
  , ("Fn1",                 keyFn1              )
  , ("Fn2",                 keyFn2              )
  , ("FnB",                 keyFnB              )
  , ("FnD",                 keyFnD              )
  , ("FnE",                 keyFnE              )
  , ("FnEsc",               keyFnEsc            )
  , ("FnF",                 keyFnF              )
  , ("FnF1",                keyFnF1             )
  , ("FnF10",               keyFnF10            )
  , ("FnF11",               keyFnF11            )
  , ("FnF12",               keyFnF12            )
  , ("FnF2",                keyFnF2             )
  , ("FnF3",                keyFnF3             )
  , ("FnF4",                keyFnF4             )
  , ("FnF5",                keyFnF5             )
  , ("FnF6",                keyFnF6             )
  , ("FnF7",                keyFnF7             )
  , ("FnF8",                keyFnF8             )
  , ("FnF9",                keyFnF9             )
  , ("FnS",                 keyFnS              )
  , ("Forward",             keyForward          )
  , ("ForwardMail",         keyForwardMail      )
  , ("FrameBack",           keyFrameBack        )
  , ("FrameForward",        keyFrameForward     )
  , ("Front",               keyFront            )
  , ("G",                   keyG                )
  , ("Games",               keyGames            )
  , ("GoTo",                keyGoTo             )
  , ("GraphicsEditor",      keyGraphicsEditor   )
  , ("Grave",               keyGrave            )
  , ("Green",               keyGreen            )
  , ("H",                   keyH                )
  , ("Hangeul",             keyHangeul          )
  , ("Hanguel",             keyHanguel          )
  , ("Hanja",               keyHanja            )
  , ("Help",                keyHelp             )
  , ("Henkan",              keyHenkan           )
  , ("Hiragana",            keyHiragana         )
  , ("Home",                keyHome             )
  , ("HomePage",            keyHomePage         )
  , ("HP",                  keyHP               )
  , ("I",                   keyI                )
  , ("Images",              keyImages           )
  , ("Info",                keyInfo             )
  , ("Insert",              keyInsert           )
  , ("InsLine",             keyInsLine          )
  , ("ISO",                 keyISO              )
  , ("J",                   keyJ                )
  , ("K",                   keyK                )
  , ("Katakana",            keyKatakana         )
  , ("KatakanaHiragana",    keyKatakanaHiragana )
  , ("KbdIllumDown",        keyKbdIllumDown     )
  , ("KbdIllumToggle",      keyKbdIllumToggle   )
  , ("KbdIllumUp",          keyKbdIllumUp       )
  , ("Keyboard",            keyKeyboard         )
  , ("Kp0",                 keyKp0              )
  , ("Kp1",                 keyKp1              )
  , ("Kp2",                 keyKp2              )
  , ("Kp3",                 keyKp3              )
  , ("Kp4",                 keyKp4              )
  , ("Kp5",                 keyKp5              )
  , ("Kp6",                 keyKp6              )
  , ("Kp7",                 keyKp7              )
  , ("Kp8",                 keyKp8              )
  , ("Kp9",                 keyKp9              )
  , ("KpAsterisk",          keyKpAsterisk       )
  , ("KpComma",             keyKpComma          )
  , ("KpDot",               keyKpDot            )
  , ("KpEnter",             keyKpEnter          )
  , ("KpEqual",             keyKpEqual          )
  , ("KpJpComma",           keyKpJpComma        )
  , ("KpLeftParen",         keyKpLeftParen      )
  , ("KpMInus",             keyKpMInus          )
  , ("KpPlus",              keyKpPlus           )
  , ("KpPlusMinus",         keyKpPlusMinus      )
  , ("KpRightParen",        keyKpRightParen     )
  , ("KpSlash",             keyKpSlash          )
  , ("L",                   keyL                )
  , ("Language",            keyLanguage         )
  , ("Last",                keyLast             )
  , ("Left",                keyLeft             )
  , ("LeftAlt",             keyLeftAlt          )
  , ("LeftBrace",           keyLeftBrace        )
  , ("LeftCtrl",            keyLeftCtrl         )
  , ("LeftMeta",            keyLeftMeta         )
  , ("LeftShift",           keyLeftShift        )
  , ("LineFeed",            keyLineFeed         )
  , ("List",                keyList             )
  , ("LogOff",              keyLogOff           )
  , ("M",                   keyM                )
  , ("Macro",               keyMacro            )
  , ("Mail",                keyMail             )
  , ("Media",               keyMedia            )
  , ("MediaRepeat",         keyMediaRepeat      )
  , ("Memo",                keyMemo             )
  , ("Menu",                keyMenu             )
  , ("Messenger",           keyMessenger        )
  , ("MHP",                 keyMHP              )
  , ("Minus",               keyMinus            )
  , ("Mode",                keyMode             )
  , ("Move",                keyMove             )
  , ("MP3",                 keyMP3              )
  , ("MsDOS",               keyMsDOS            )
  , ("Muhenkan",            keyMuhenkan         )
  , ("Mute",                keyMute             )
  , ("N",                   keyN                )
  , ("New",                 keyNew              )
  , ("News",                keyNews             )
  , ("Next",                keyNext             )
  , ("NextSong",            keyNextSong         )
  , ("Numeric0",            keyNumeric0         )
  , ("Numeric1",            keyNumeric1         )
  , ("Numeric2",            keyNumeric2         )
  , ("Numeric3",            keyNumeric3         )
  , ("Numeric4",            keyNumeric4         )
  , ("Numeric5",            keyNumeric5         )
  , ("Numeric6",            keyNumeric6         )
  , ("Numeric7",            keyNumeric7         )
  , ("Numeric8",            keyNumeric8         )
  , ("Numeric9",            keyNumeric9         )
  , ("NumericPound",        keyNumericPound     )
  , ("NumericStar",         keyNumericStar      )
  , ("NumLock",             keyNumLock          )
  , ("O",                   keyO                )
  , ("Ok",                  keyOk               )
  , ("Open",                keyOpen             )
  , ("Option",              keyOption           )
  , ("P",                   keyP                )
  , ("PageDown",            keyPageDown         )
  , ("PageUp",              keyPageUp           )
  , ("Paste",               keyPaste            )
  , ("Pause",               keyPause            )
  , ("PauseCD",             keyPauseCD          )
  , ("PC",                  keyPC               )
  , ("Phone",               keyPhone            )
  , ("Play",                keyPlay             )
  , ("PlayCD",              keyPlayCD           )
  , ("Player",              keyPlayer           )
  , ("PlayPause",           keyPlayPause        )
  , ("Power",               keyPower            )
  , ("Power2",              keyPower2           )
  , ("Presentation",        keyPresentation     )
  , ("Previous",            keyPrevious         )
  , ("PreviousSong",        keyPreviousSong     )
  , ("Print",               keyPrint            )
  , ("Prog1",               keyProg1            )
  , ("Prog2",               keyProg2            )
  , ("Prog3",               keyProg3            )
  , ("Prog4",               keyProg4            )
  , ("Program",             keyProgram          )
  , ("Props",               keyProps            )
  , ("PVR",                 keyPVR              )
  , ("Q",                   keyQ                )
  , ("Question",            keyQuestion         )
  , ("R",                   keyR                )
  , ("Radio",               keyRadio            )
  , ("Record",              keyRecord           )
  , ("Red",                 keyRed              )
  , ("Redo",                keyRedo             )
  , ("Refresh",             keyRefresh          )
  , ("Reply",               keyReply            )
  , ("Reserved",            keyReserved         )
  , ("Restart",             keyRestart          )
  , ("Rewind",              keyRewind           )
  , ("RFKill",              keyRFKill           )
  , ("Right",               keyRight            )
  , ("RightAlt",            keyRightAlt         )
  , ("RightBrace",          keyRightBrace       )
  , ("RightCtrl",           keyRightCtrl        )
  , ("RightMeta",           keyRightMeta        )
  , ("RightShift",          keyRightShift       )
  , ("Ro",                  keyRo               )
  , ("S",                   keyS                )
  , ("SAT",                 keySAT              )
  , ("SAT2",                keySAT2             )
  , ("Save",                keySave             )
  , ("Scale",               keyScale            )
  , ("Screen",              keyScreen           )
  , ("ScreenLock",          keyScreenLock       )
  , ("ScrollDown",          keyScrollDown       )
  , ("ScrollLock",          keyScrollLock       )
  , ("ScrollUp",            keyScrollUp         )
  , ("Search",              keySearch           )
  , ("Select",              keySelect           )
  , ("SemiColon",           keySemiColon        )
  , ("Send",                keySend             )
  , ("SendFile",            keySendFile         )
  , ("Setup",               keySetup            )
  , ("Shop",                keyShop             )
  , ("Shuffle",             keyShuffle          )
  , ("Slash",               keySlash            )
  , ("Sleep",               keySleep            )
  , ("Slow",                keySlow             )
  , ("Sound",               keySound            )
  , ("Space",               keySpace            )
  , ("SpellCheck",          keySpellCheck       )
  , ("Sport",               keySport            )
  , ("Spreadsheet",         keySpreadsheet      )
  , ("Stop",                keyStop             )
  , ("StopCD",              keyStopCD           )
  , ("SubTitle",            keySubTitle         )
  , ("Suspend",             keySuspend          )
  , ("SwitchVideoMode",     keySwitchVideoMode  )
  , ("SysRq",               keySysRq            )
  , ("T",                   keyT                )
  , ("tab",                 keytab              )
  , ("Tape",                keyTape             )
  , ("Teen",                keyTeen             )
  , ("Text",                keyText             )
  , ("Time",                keyTime             )
  , ("Title",               keyTitle            )
  , ("TouchpadOff",         keyTouchpadOff      )
  , ("TouchpadOn",          keyTouchpadOn       )
  , ("TouchpadToggle",      keyTouchpadToggle   )
  , ("Tuner",               keyTuner            )
  , ("TV",                  keyTV               )
  , ("TV2",                 keyTV2              )
  , ("Twen",                keyTwen             )
  , ("U",                   keyU                )
  , ("Undo",                keyUndo             )
  , ("Unknown",             keyUnknown          )
  , ("Up",                  keyUp               )
  , ("UWB",                 keyUWB              )
  , ("V",                   keyV                )
  , ("VCR",                 keyVCR              )
  , ("VCR2",                keyVCR2             )
  , ("Vendor",              keyVendor           )
  , ("Video",               keyVideo            )
  , ("VideoPhone",          keyVideoPhone       )
  , ("VideoNext",           keyVideoNext        )
  , ("VideoPrev",           keyVideoPrev        )
  , ("VoiceMail",           keyVoiceMail        )
  , ("VolumeDown",          keyVolumeDown       )
  , ("VolumeUp",            keyVolumeUp         )
  , ("W",                   keyW                )
  , ("WakeUp",              keyWakeUp           )
  , ("WiMax",               keyWiMax            )
  , ("WLAN",                keyWLAN             )
  , ("WordProcessor",       keyWordProcessor    )
  , ("WPSButton",           keyWPSButton        )
  , ("WWW",                 keyWWW              )
  , ("X",                   keyX                )
  , ("XFer",                keyXFer             )
  , ("Y",                   keyY                )
  , ("Yellow",              keyYellow           )
  , ("Yen",                 keyYen              )
  , ("Z",                   keyZ                )
  , ("ZenkakuHankaku",      keyZenkakuHankaku   )
  , ("Zoom",                keyZoom             )
  , ("ZoomIn",              keyZoomIn           )
  , ("ZoomOut",             keyZoomOut          )
  , ("ZoomReset",           keyZoomReset        )
  ]

{- ########################################################################################## -}
