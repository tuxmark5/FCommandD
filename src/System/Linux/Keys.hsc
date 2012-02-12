{-# LANGUAGE CPP #-}

module System.Linux.Keys where
{- ########################################################################################## -}
import Data.Word (Word16)
{- ########################################################################################## -}
#include <linux/input.h>
{- ########################################################################################## -}

btn0                :: Word16
btn1                :: Word16
btn2                :: Word16
btn3                :: Word16
btn4                :: Word16
btn5                :: Word16
btn6                :: Word16
btn7                :: Word16
btn8                :: Word16
btn9                :: Word16
btnA                :: Word16
btnB                :: Word16
btnBack             :: Word16
btnBase             :: Word16
btnBase2            :: Word16
btnBase3            :: Word16
btnBase4            :: Word16
btnBase5            :: Word16
btnBase6            :: Word16
btnC                :: Word16
btnDead             :: Word16
btnDigi             :: Word16
btnExtra            :: Word16
btnForward          :: Word16
btnGamePad          :: Word16
btnGearDown         :: Word16
btnGearUp           :: Word16
btnJoystick         :: Word16
btnLeft             :: Word16
btnMiddle           :: Word16
btnMisc             :: Word16
btnMode             :: Word16
btnMouse            :: Word16
btnPinkie           :: Word16
btnRight            :: Word16
btnSelect           :: Word16
btnSide             :: Word16
btnStart            :: Word16
btnStylus           :: Word16
btnStylus2          :: Word16
btnTask             :: Word16
btnThumb            :: Word16
btnThumb2           :: Word16
btnThumbL           :: Word16
btnThumbR           :: Word16
btnTL               :: Word16
btnTL2              :: Word16
btnToolAirbrush     :: Word16
btnToolBrush        :: Word16
btnToolDoubleTap    :: Word16
btnToolFinger       :: Word16
btnToolLens         :: Word16
btnToolMouse        :: Word16
btnToolPen          :: Word16
btnToolPencil       :: Word16
btnToolQuadTap      :: Word16
btnToolRubber       :: Word16
btnToolTripleTap    :: Word16
btnTop              :: Word16
btnTop2             :: Word16
btnTouch            :: Word16
btnTR               :: Word16
btnTR2              :: Word16
btnTrigger          :: Word16
btnTriggerHappy     :: Word16
btnTriggerHappy1    :: Word16
btnTriggerHappy10   :: Word16
btnTriggerHappy11   :: Word16
btnTriggerHappy12   :: Word16
btnTriggerHappy13   :: Word16
btnTriggerHappy14   :: Word16
btnTriggerHappy15   :: Word16
btnTriggerHappy16   :: Word16
btnTriggerHappy17   :: Word16
btnTriggerHappy18   :: Word16
btnTriggerHappy19   :: Word16
btnTriggerHappy2    :: Word16
btnTriggerHappy20   :: Word16
btnTriggerHappy21   :: Word16
btnTriggerHappy22   :: Word16
btnTriggerHappy23   :: Word16
btnTriggerHappy24   :: Word16
btnTriggerHappy25   :: Word16
btnTriggerHappy26   :: Word16
btnTriggerHappy27   :: Word16
btnTriggerHappy28   :: Word16
btnTriggerHappy29   :: Word16
btnTriggerHappy3    :: Word16
btnTriggerHappy30   :: Word16
btnTriggerHappy31   :: Word16
btnTriggerHappy32   :: Word16
btnTriggerHappy33   :: Word16
btnTriggerHappy34   :: Word16
btnTriggerHappy35   :: Word16
btnTriggerHappy36   :: Word16
btnTriggerHappy37   :: Word16
btnTriggerHappy38   :: Word16
btnTriggerHappy39   :: Word16
btnTriggerHappy4    :: Word16
btnTriggerHappy40   :: Word16
btnTriggerHappy5    :: Word16
btnTriggerHappy6    :: Word16
btnTriggerHappy7    :: Word16
btnTriggerHappy8    :: Word16
btnTriggerHappy9    :: Word16
btnWheel            :: Word16
btnX                :: Word16
btnY                :: Word16
btnZ                :: Word16

{- ########################################################################################## -}

key0                :: Word16
key1                :: Word16
key102nd            :: Word16
key10ChannelsDown   :: Word16
key10ChannelsUp     :: Word16
key2                :: Word16
key3                :: Word16
key4                :: Word16
key5                :: Word16
key6                :: Word16
key7                :: Word16
key8                :: Word16
key9                :: Word16
keyA                :: Word16
keyAB               :: Word16
keyAddressBook      :: Word16
keyAgain            :: Word16
keyAlterase         :: Word16
keyAngle            :: Word16
keyApostrophe       :: Word16
keyArchive          :: Word16
keyAudio            :: Word16
keyAux              :: Word16
keyB                :: Word16
keyBack             :: Word16
keyBackslash        :: Word16
keyBackspace        :: Word16
keyBassBoost        :: Word16
keyBattery          :: Word16
keyBlue             :: Word16
keyBluetooth        :: Word16
keyBookMarks        :: Word16
keyBreak            :: Word16
keyBrightnessDown   :: Word16
keyBrightnessUp     :: Word16
keyBrightnessCycle  :: Word16
keyBrightnessZero   :: Word16
keyBrlDot1          :: Word16
keyBrlDot10         :: Word16
keyBrlDot2          :: Word16
keyBrlDot3          :: Word16
keyBrlDot4          :: Word16
keyBrlDot5          :: Word16
keyBrlDot6          :: Word16
keyBrlDot7          :: Word16
keyBrlDot8          :: Word16
keyBrlDot9          :: Word16
keyC                :: Word16
keyCalc             :: Word16
keyCalendar         :: Word16
keyCamera           :: Word16
keyCameraDown       :: Word16
keyCameraFocus      :: Word16
keyCameraLeft       :: Word16
keyCameraRight      :: Word16
keyCameraUp         :: Word16
keyCameraZoomIn     :: Word16
keyCameraZoomOut    :: Word16
keyCancel           :: Word16
keyCapsLock         :: Word16
keyCD               :: Word16
keyChannel          :: Word16
keyChannelDown      :: Word16
keyChannelUp        :: Word16
keyChat             :: Word16
keyClear            :: Word16
keyClose            :: Word16
keyCloseCD          :: Word16
keyCoffee           :: Word16
keyComma            :: Word16
keyCompose          :: Word16
keyComputer         :: Word16
keyConfig           :: Word16
keyConnect          :: Word16
keyContactMenu      :: Word16
keyCopy             :: Word16
keyCut              :: Word16
keyCycleWindows     :: Word16
keyD                :: Word16
keyBashboard        :: Word16
keyDatabase         :: Word16
keyDelete           :: Word16
keyDeleteFile       :: Word16
keyDelEOL           :: Word16
keyDelEOS           :: Word16
keyDelLine          :: Word16
keyDigits           :: Word16
keyDirection        :: Word16
keyDirectory        :: Word16
keyDisplayToggle    :: Word16
keyDisplayOff       :: Word16
keyDocuments        :: Word16
keyDollar           :: Word16
keyDot              :: Word16
keyDown             :: Word16
keyDVD              :: Word16
keyE                :: Word16
keyEdit             :: Word16
keyEditor           :: Word16
keyEjectCT          :: Word16
keyEjectCloseCD     :: Word16
keyEmail            :: Word16
keyEnd              :: Word16
keyEnter            :: Word16
keyEPG              :: Word16
keyEqual            :: Word16
keyESC              :: Word16
keyEuro             :: Word16
keyExit             :: Word16
keyF                :: Word16
keyF1               :: Word16
keyF10              :: Word16
keyF11              :: Word16
keyF12              :: Word16
keyF13              :: Word16
keyF14              :: Word16
keyF15              :: Word16
keyF16              :: Word16
keyF17              :: Word16
keyF18              :: Word16
keyF19              :: Word16
keyF2               :: Word16
keyF20              :: Word16
keyF21              :: Word16
keyF22              :: Word16
keyF23              :: Word16
keyF24              :: Word16
keyF3               :: Word16
keyF4               :: Word16
keyF5               :: Word16
keyF6               :: Word16
keyF7               :: Word16
keyF8               :: Word16
keyF9               :: Word16
keyFastForward      :: Word16
keyFavorites        :: Word16
keyFile             :: Word16
keyFinance          :: Word16
keyFind             :: Word16
keyFirst            :: Word16
keyFn               :: Word16
keyFn1              :: Word16
keyFn2              :: Word16
keyFnB              :: Word16
keyFnD              :: Word16
keyFnE              :: Word16
keyFnEsc            :: Word16
keyFnF              :: Word16
keyFnF1             :: Word16
keyFnF10            :: Word16
keyFnF11            :: Word16
keyFnF12            :: Word16
keyFnF2             :: Word16
keyFnF3             :: Word16
keyFnF4             :: Word16
keyFnF5             :: Word16
keyFnF6             :: Word16
keyFnF7             :: Word16
keyFnF8             :: Word16
keyFnF9             :: Word16
keyFnS              :: Word16
keyForward          :: Word16
keyForwardMail      :: Word16
keyFrameBack        :: Word16
keyFrameForward     :: Word16
keyFront            :: Word16
keyG                :: Word16
keyGames            :: Word16
keyGoTo             :: Word16
keyGraphicsEditor   :: Word16
keyGrave            :: Word16
keyGreen            :: Word16
keyH                :: Word16
keyHangeul          :: Word16
keyHanguel          :: Word16
keyHanja            :: Word16
keyHelp             :: Word16
keyHenkan           :: Word16
keyHiragana         :: Word16
keyHome             :: Word16
keyHomePage         :: Word16
keyHP               :: Word16
keyI                :: Word16
keyImages           :: Word16
keyInfo             :: Word16
keyInsert           :: Word16
keyInsLine          :: Word16
keyISO              :: Word16
keyJ                :: Word16
keyK                :: Word16
keyKatakana         :: Word16
keyKatakanaHiragana :: Word16
keyKbdIllumDown     :: Word16
keyKbdIllumToggle   :: Word16
keyKbdIllumUp       :: Word16
keyKeyboard         :: Word16
keyKp0              :: Word16
keyKp1              :: Word16
keyKp2              :: Word16
keyKp3              :: Word16
keyKp4              :: Word16
keyKp5              :: Word16
keyKp6              :: Word16
keyKp7              :: Word16
keyKp8              :: Word16
keyKp9              :: Word16
keyKpAsterisk       :: Word16
keyKpComma          :: Word16
keyKpDot            :: Word16
keyKpEnter          :: Word16
keyKpEqual          :: Word16
keyKpJpComma        :: Word16
keyKpLeftParen      :: Word16
keyKpMInus          :: Word16
keyKpPlus           :: Word16
keyKpPlusMinus      :: Word16
keyKpRightParen     :: Word16
keyKpSlash          :: Word16
keyL                :: Word16
keyLanguage         :: Word16
keyLast             :: Word16
keyLeft             :: Word16
keyLeftAlt          :: Word16
keyLeftBrace        :: Word16
keyLeftCtrl         :: Word16
keyLeftMeta         :: Word16
keyLeftShift        :: Word16
keyLineFeed         :: Word16
keyList             :: Word16
keyLogOff           :: Word16
keyM                :: Word16
keyMacro            :: Word16
keyMail             :: Word16
keyMedia            :: Word16
keyMediaRepeat      :: Word16
keyMemo             :: Word16
keyMenu             :: Word16
keyMessenger        :: Word16
keyMHP              :: Word16
keyMinus            :: Word16
keyMode             :: Word16
keyMove             :: Word16
keyMP3              :: Word16
keyMsDOS            :: Word16
keyMuhenkan         :: Word16
keyMute             :: Word16
keyN                :: Word16
keyNew              :: Word16
keyNews             :: Word16
keyNext             :: Word16
keyNextSong         :: Word16
keyNumeric0         :: Word16
keyNumeric1         :: Word16
keyNumeric2         :: Word16
keyNumeric3         :: Word16
keyNumeric4         :: Word16
keyNumeric5         :: Word16
keyNumeric6         :: Word16
keyNumeric7         :: Word16
keyNumeric8         :: Word16
keyNumeric9         :: Word16
keyNumericPound     :: Word16
keyNumericStar      :: Word16
keyNumLock          :: Word16
keyO                :: Word16
keyOk               :: Word16
keyOpen             :: Word16
keyOption           :: Word16
keyP                :: Word16
keyPageDown         :: Word16
keyPageUp           :: Word16
keyPaste            :: Word16
keyPause            :: Word16
keyPauseCD          :: Word16
keyPC               :: Word16
keyPhone            :: Word16
keyPlay             :: Word16
keyPlayCD           :: Word16
keyPlayer           :: Word16
keyPlayPause        :: Word16
keyPower            :: Word16
keyPower2           :: Word16
keyPresentation     :: Word16
keyPrevious         :: Word16
keyPreviousSong     :: Word16
keyPrint            :: Word16
keyProg1            :: Word16
keyProg2            :: Word16
keyProg3            :: Word16
keyProg4            :: Word16
keyProgram          :: Word16
keyProps            :: Word16
keyPVR              :: Word16
keyQ                :: Word16
keyQuestion         :: Word16
keyR                :: Word16
keyRadio            :: Word16
keyRecord           :: Word16
keyRed              :: Word16
keyRedo             :: Word16
keyRefresh          :: Word16
keyReply            :: Word16
keyReserved         :: Word16
keyRestart          :: Word16
keyRewind           :: Word16
keyRFKill           :: Word16
keyRight            :: Word16
keyRightAlt         :: Word16
keyRightBrace       :: Word16
keyRightCtrl        :: Word16
keyRightMeta        :: Word16
keyRightShift       :: Word16
keyRo               :: Word16
keyS                :: Word16
keySAT              :: Word16
keySAT2             :: Word16
keySave             :: Word16
keyScale            :: Word16
keyScreen           :: Word16
keyScreenLock       :: Word16
keyScrollDown       :: Word16
keyScrollLock       :: Word16
keyScrollUp         :: Word16
keySearch           :: Word16
keySelect           :: Word16
keySemiColon        :: Word16
keySend             :: Word16
keySendFile         :: Word16
keySetup            :: Word16
keyShop             :: Word16
keyShuffle          :: Word16
keySlash            :: Word16
keySleep            :: Word16
keySlow             :: Word16
keySound            :: Word16
keySpace            :: Word16
keySpellCheck       :: Word16
keySport            :: Word16
keySpreadsheet      :: Word16
keyStop             :: Word16
keyStopCD           :: Word16
keySubTitle         :: Word16
keySuspend          :: Word16
keySwitchVideoMode  :: Word16
keySysRq            :: Word16
keyT                :: Word16
keytab              :: Word16
keyTape             :: Word16
keyTeen             :: Word16
keyText             :: Word16
keyTime             :: Word16
keyTitle            :: Word16
keyTouchpadOff      :: Word16
keyTouchpadOn       :: Word16
keyTouchpadToggle   :: Word16
keyTuner            :: Word16
keyTV               :: Word16
keyTV2              :: Word16
keyTwen             :: Word16
keyU                :: Word16
keyUndo             :: Word16
keyUnknown          :: Word16
keyUp               :: Word16
keyUWB              :: Word16
keyV                :: Word16
keyVCR              :: Word16
keyVCR2             :: Word16
keyVendor           :: Word16
keyVideo            :: Word16
keyVideoPhone       :: Word16
keyVideoNext        :: Word16
keyVideoPrev        :: Word16
keyVoiceMail        :: Word16
keyVolumeDown       :: Word16
keyVolumeUp         :: Word16
keyW                :: Word16
keyWakeUp           :: Word16
keyWiMax            :: Word16
keyWLAN             :: Word16
keyWordProcessor    :: Word16
keyWPSButton        :: Word16
keyWWW              :: Word16
keyX                :: Word16
keyXFer             :: Word16
keyY                :: Word16
keyYellow           :: Word16
keyYen              :: Word16
keyZ                :: Word16
keyZenkakuHankaku   :: Word16
keyZoom             :: Word16
keyZoomIn           :: Word16
keyZoomOut          :: Word16
keyZoomReset        :: Word16

{- ########################################################################################## -}

keyReserved         = #const KEY_RESERVED
keyESC              = #const KEY_ESC
key1                = #const KEY_1
key2                = #const KEY_2
key3                = #const KEY_3
key4                = #const KEY_4
key5                = #const KEY_5
key6                = #const KEY_6
key7                = #const KEY_7
key8                = #const KEY_8
key9                = #const KEY_9
key0                = #const KEY_0
keyMinus            = #const KEY_MINUS
keyEqual            = #const KEY_EQUAL
keyBackspace        = #const KEY_BACKSPACE
keytab              = #const KEY_TAB
keyQ                = #const KEY_Q
keyW                = #const KEY_W
keyE                = #const KEY_E
keyR                = #const KEY_R
keyT                = #const KEY_T
keyY                = #const KEY_Y
keyU                = #const KEY_U
keyI                = #const KEY_I
keyO                = #const KEY_O
keyP                = #const KEY_P
keyLeftBrace        = #const KEY_LEFTBRACE
keyRightBrace       = #const KEY_RIGHTBRACE
keyEnter            = #const KEY_ENTER
keyLeftCtrl         = #const KEY_LEFTCTRL
keyA                = #const KEY_A
keyS                = #const KEY_S
keyD                = #const KEY_D
keyF                = #const KEY_F
keyG                = #const KEY_G
keyH                = #const KEY_H
keyJ                = #const KEY_J
keyK                = #const KEY_K
keyL                = #const KEY_L
keySemiColon        = #const KEY_SEMICOLON
keyApostrophe       = #const KEY_APOSTROPHE
keyGrave            = #const KEY_GRAVE
keyLeftShift        = #const KEY_LEFTSHIFT
keyBackslash        = #const KEY_BACKSLASH
keyZ                = #const KEY_Z
keyX                = #const KEY_X
keyC                = #const KEY_C
keyV                = #const KEY_V
keyB                = #const KEY_B
keyN                = #const KEY_N
keyM                = #const KEY_M
keyComma            = #const KEY_COMMA
keyDot              = #const KEY_DOT
keySlash            = #const KEY_SLASH
keyRightShift       = #const KEY_RIGHTSHIFT
keyKpAsterisk       = #const KEY_KPASTERISK
keyLeftAlt          = #const KEY_LEFTALT
keySpace            = #const KEY_SPACE
keyCapsLock         = #const KEY_CAPSLOCK
keyF1               = #const KEY_F1
keyF2               = #const KEY_F2
keyF3               = #const KEY_F3
keyF4               = #const KEY_F4
keyF5               = #const KEY_F5
keyF6               = #const KEY_F6
keyF7               = #const KEY_F7
keyF8               = #const KEY_F8
keyF9               = #const KEY_F9
keyF10              = #const KEY_F10
keyNumLock          = #const KEY_NUMLOCK
keyScrollLock       = #const KEY_SCROLLLOCK
keyKp7              = #const KEY_KP7
keyKp8              = #const KEY_KP8
keyKp9              = #const KEY_KP9
keyKpMInus          = #const KEY_KPMINUS
keyKp4              = #const KEY_KP4
keyKp5              = #const KEY_KP5
keyKp6              = #const KEY_KP6
keyKpPlus           = #const KEY_KPPLUS
keyKp1              = #const KEY_KP1
keyKp2              = #const KEY_KP2
keyKp3              = #const KEY_KP3
keyKp0              = #const KEY_KP0
keyKpDot            = #const KEY_KPDOT
keyZenkakuHankaku   = #const KEY_ZENKAKUHANKAKU
key102nd            = #const KEY_102ND
keyF11              = #const KEY_F11
keyF12              = #const KEY_F12
keyRo               = #const KEY_RO
keyKatakana         = #const KEY_KATAKANA
keyHiragana         = #const KEY_HIRAGANA
keyHenkan           = #const KEY_HENKAN
keyKatakanaHiragana = #const KEY_KATAKANAHIRAGANA
keyMuhenkan         = #const KEY_MUHENKAN
keyKpJpComma        = #const KEY_KPJPCOMMA
keyKpEnter          = #const KEY_KPENTER
keyRightCtrl        = #const KEY_RIGHTCTRL
keyKpSlash          = #const KEY_KPSLASH
keySysRq            = #const KEY_SYSRQ
keyRightAlt         = #const KEY_RIGHTALT
keyLineFeed         = #const KEY_LINEFEED
keyHome             = #const KEY_HOME
keyUp               = #const KEY_UP
keyPageUp           = #const KEY_PAGEUP
keyLeft             = #const KEY_LEFT
keyRight            = #const KEY_RIGHT
keyEnd              = #const KEY_END
keyDown             = #const KEY_DOWN
keyPageDown         = #const KEY_PAGEDOWN
keyInsert           = #const KEY_INSERT
keyDelete           = #const KEY_DELETE
keyMacro            = #const KEY_MACRO
keyMute             = #const KEY_MUTE
keyVolumeDown       = #const KEY_VOLUMEDOWN
keyVolumeUp         = #const KEY_VOLUMEUP
keyPower            = #const KEY_POWER
keyKpEqual          = #const KEY_KPEQUAL
keyKpPlusMinus      = #const KEY_KPPLUSMINUS
keyPause            = #const KEY_PAUSE
keyScale            = #const KEY_SCALE
keyKpComma          = #const KEY_KPCOMMA
keyHangeul          = #const KEY_HANGEUL
keyHanguel          = #const KEY_HANGUEL
keyHanja            = #const KEY_HANJA
keyYen              = #const KEY_YEN
keyLeftMeta         = #const KEY_LEFTMETA
keyRightMeta        = #const KEY_RIGHTMETA
keyCompose          = #const KEY_COMPOSE
keyStop             = #const KEY_STOP
keyAgain            = #const KEY_AGAIN
keyProps            = #const KEY_PROPS
keyUndo             = #const KEY_UNDO
keyFront            = #const KEY_FRONT
keyCopy             = #const KEY_COPY
keyOpen             = #const KEY_OPEN
keyPaste            = #const KEY_PASTE
keyFind             = #const KEY_FIND
keyCut              = #const KEY_CUT
keyHelp             = #const KEY_HELP
keyMenu             = #const KEY_MENU
keyCalc             = #const KEY_CALC
keySetup            = #const KEY_SETUP
keySleep            = #const KEY_SLEEP
keyWakeUp           = #const KEY_WAKEUP
keyFile             = #const KEY_FILE
keySendFile         = #const KEY_SENDFILE
keyDeleteFile       = #const KEY_DELETEFILE
keyXFer             = #const KEY_XFER
keyProg1            = #const KEY_PROG1
keyProg2            = #const KEY_PROG2
keyWWW              = #const KEY_WWW
keyMsDOS            = #const KEY_MSDOS
keyCoffee           = #const KEY_COFFEE
keyScreenLock       = #const KEY_SCREENLOCK
keyDirection        = #const KEY_DIRECTION
keyCycleWindows     = #const KEY_CYCLEWINDOWS
keyMail             = #const KEY_MAIL
keyBookMarks        = #const KEY_BOOKMARKS
keyComputer         = #const KEY_COMPUTER
keyBack             = #const KEY_BACK
keyForward          = #const KEY_FORWARD
keyCloseCD          = #const KEY_CLOSECD
keyEjectCT          = #const KEY_EJECTCD
keyEjectCloseCD     = #const KEY_EJECTCLOSECD
keyNextSong         = #const KEY_NEXTSONG
keyPlayPause        = #const KEY_PLAYPAUSE
keyPreviousSong     = #const KEY_PREVIOUSSONG
keyStopCD           = #const KEY_STOPCD
keyRecord           = #const KEY_RECORD
keyRewind           = #const KEY_REWIND
keyPhone            = #const KEY_PHONE
keyISO              = #const KEY_ISO
keyConfig           = #const KEY_CONFIG
keyHomePage         = #const KEY_HOMEPAGE
keyRefresh          = #const KEY_REFRESH
keyExit             = #const KEY_EXIT
keyMove             = #const KEY_MOVE
keyEdit             = #const KEY_EDIT
keyScrollUp         = #const KEY_SCROLLUP
keyScrollDown       = #const KEY_SCROLLDOWN
keyKpLeftParen      = #const KEY_KPLEFTPAREN
keyKpRightParen     = #const KEY_KPRIGHTPAREN
keyNew              = #const KEY_NEW
keyRedo             = #const KEY_REDO
keyF13              = #const KEY_F13
keyF14              = #const KEY_F14
keyF15              = #const KEY_F15
keyF16              = #const KEY_F16
keyF17              = #const KEY_F17
keyF18              = #const KEY_F18
keyF19              = #const KEY_F19
keyF20              = #const KEY_F20
keyF21              = #const KEY_F21
keyF22              = #const KEY_F22
keyF23              = #const KEY_F23
keyF24              = #const KEY_F24
keyPlayCD           = #const KEY_PLAYCD
keyPauseCD          = #const KEY_PAUSECD
keyProg3            = #const KEY_PROG3
keyProg4            = #const KEY_PROG4
keyBashboard        = #const KEY_DASHBOARD
keySuspend          = #const KEY_SUSPEND
keyClose            = #const KEY_CLOSE
keyPlay             = #const KEY_PLAY
keyFastForward      = #const KEY_FASTFORWARD
keyBassBoost        = #const KEY_BASSBOOST
keyPrint            = #const KEY_PRINT
keyHP               = #const KEY_HP
keyCamera           = #const KEY_CAMERA
keySound            = #const KEY_SOUND
keyQuestion         = #const KEY_QUESTION
keyEmail            = #const KEY_EMAIL
keyChat             = #const KEY_CHAT
keySearch           = #const KEY_SEARCH
keyConnect          = #const KEY_CONNECT
keyFinance          = #const KEY_FINANCE
keySport            = #const KEY_SPORT
keyShop             = #const KEY_SHOP
keyAlterase         = #const KEY_ALTERASE
keyCancel           = #const KEY_CANCEL
keyBrightnessDown   = #const KEY_BRIGHTNESSDOWN
keyBrightnessUp     = #const KEY_BRIGHTNESSUP
keyMedia            = #const KEY_MEDIA
keySwitchVideoMode  = #const KEY_SWITCHVIDEOMODE
keyKbdIllumToggle   = #const KEY_KBDILLUMTOGGLE
keyKbdIllumDown     = #const KEY_KBDILLUMDOWN
keyKbdIllumUp       = #const KEY_KBDILLUMUP
keySend             = #const KEY_SEND
keyReply            = #const KEY_REPLY
keyForwardMail      = #const KEY_FORWARDMAIL
keySave             = #const KEY_SAVE
keyDocuments        = #const KEY_DOCUMENTS
keyBattery          = #const KEY_BATTERY
keyBluetooth        = #const KEY_BLUETOOTH
keyWLAN             = #const KEY_WLAN
keyUWB              = #const KEY_UWB
keyUnknown          = #const KEY_UNKNOWN
keyVideoNext        = #const KEY_VIDEO_NEXT
keyVideoPrev        = #const KEY_VIDEO_PREV
keyBrightnessCycle  = #const KEY_BRIGHTNESS_CYCLE
keyBrightnessZero   = #const KEY_BRIGHTNESS_ZERO
keyDisplayOff       = #const KEY_DISPLAY_OFF
keyWiMax            = #const KEY_WIMAX
keyRFKill           = #const KEY_RFKILL
btnMisc             = #const BTN_MISC
btn0                = #const BTN_0
btn1                = #const BTN_1
btn2                = #const BTN_2
btn3                = #const BTN_3
btn4                = #const BTN_4
btn5                = #const BTN_5
btn6                = #const BTN_6
btn7                = #const BTN_7
btn8                = #const BTN_8
btn9                = #const BTN_9
btnMouse            = #const BTN_MOUSE
btnLeft             = #const BTN_LEFT
btnRight            = #const BTN_RIGHT
btnMiddle           = #const BTN_MIDDLE
btnSide             = #const BTN_SIDE
btnExtra            = #const BTN_EXTRA
btnForward          = #const BTN_FORWARD
btnBack             = #const BTN_BACK
btnTask             = #const BTN_TASK
btnJoystick         = #const BTN_JOYSTICK
btnTrigger          = #const BTN_TRIGGER
btnThumb            = #const BTN_THUMB
btnThumb2           = #const BTN_THUMB2
btnTop              = #const BTN_TOP
btnTop2             = #const BTN_TOP2
btnPinkie           = #const BTN_PINKIE
btnBase             = #const BTN_BASE
btnBase2            = #const BTN_BASE2
btnBase3            = #const BTN_BASE3
btnBase4            = #const BTN_BASE4
btnBase5            = #const BTN_BASE5
btnBase6            = #const BTN_BASE6
btnDead             = #const BTN_DEAD
btnGamePad          = #const BTN_GAMEPAD
btnA                = #const BTN_A
btnB                = #const BTN_B
btnC                = #const BTN_C
btnX                = #const BTN_X
btnY                = #const BTN_Y
btnZ                = #const BTN_Z
btnTL               = #const BTN_TL
btnTR               = #const BTN_TR
btnTL2              = #const BTN_TL2
btnTR2              = #const BTN_TR2
btnSelect           = #const BTN_SELECT
btnStart            = #const BTN_START
btnMode             = #const BTN_MODE
btnThumbL           = #const BTN_THUMBL
btnThumbR           = #const BTN_THUMBR
btnDigi             = #const BTN_DIGI
btnToolPen          = #const BTN_TOOL_PEN
btnToolRubber       = #const BTN_TOOL_RUBBER
btnToolBrush        = #const BTN_TOOL_BRUSH
btnToolPencil       = #const BTN_TOOL_PENCIL
btnToolAirbrush     = #const BTN_TOOL_AIRBRUSH
btnToolFinger       = #const BTN_TOOL_FINGER
btnToolMouse        = #const BTN_TOOL_MOUSE
btnToolLens         = #const BTN_TOOL_LENS
btnTouch            = #const BTN_TOUCH
btnStylus           = #const BTN_STYLUS
btnStylus2          = #const BTN_STYLUS2
btnToolDoubleTap    = #const BTN_TOOL_DOUBLETAP
btnToolTripleTap    = #const BTN_TOOL_TRIPLETAP
btnToolQuadTap      = #const BTN_TOOL_QUADTAP
btnWheel            = #const BTN_WHEEL
btnGearDown         = #const BTN_GEAR_DOWN
btnGearUp           = #const BTN_GEAR_UP
keyOk               = #const KEY_OK
keySelect           = #const KEY_SELECT
keyGoTo             = #const KEY_GOTO
keyClear            = #const KEY_CLEAR
keyPower2           = #const KEY_POWER2
keyOption           = #const KEY_OPTION
keyInfo             = #const KEY_INFO
keyTime             = #const KEY_TIME
keyVendor           = #const KEY_VENDOR
keyArchive          = #const KEY_ARCHIVE
keyProgram          = #const KEY_PROGRAM
keyChannel          = #const KEY_CHANNEL
keyFavorites        = #const KEY_FAVORITES
keyEPG              = #const KEY_EPG
keyPVR              = #const KEY_PVR
keyMHP              = #const KEY_MHP
keyLanguage         = #const KEY_LANGUAGE
keyTitle            = #const KEY_TITLE
keySubTitle         = #const KEY_SUBTITLE
keyAngle            = #const KEY_ANGLE
keyZoom             = #const KEY_ZOOM
keyMode             = #const KEY_MODE
keyKeyboard         = #const KEY_KEYBOARD
keyScreen           = #const KEY_SCREEN
keyPC               = #const KEY_PC
keyTV               = #const KEY_TV
keyTV2              = #const KEY_TV2
keyVCR              = #const KEY_VCR
keyVCR2             = #const KEY_VCR2
keySAT              = #const KEY_SAT
keySAT2             = #const KEY_SAT2
keyCD               = #const KEY_CD
keyTape             = #const KEY_TAPE
keyRadio            = #const KEY_RADIO
keyTuner            = #const KEY_TUNER
keyPlayer           = #const KEY_PLAYER
keyText             = #const KEY_TEXT
keyDVD              = #const KEY_DVD
keyAux              = #const KEY_AUX
keyMP3              = #const KEY_MP3
keyAudio            = #const KEY_AUDIO
keyVideo            = #const KEY_VIDEO
keyDirectory        = #const KEY_DIRECTORY
keyList             = #const KEY_LIST
keyMemo             = #const KEY_MEMO
keyCalendar         = #const KEY_CALENDAR
keyRed              = #const KEY_RED
keyGreen            = #const KEY_GREEN
keyYellow           = #const KEY_YELLOW
keyBlue             = #const KEY_BLUE
keyChannelUp        = #const KEY_CHANNELUP
keyChannelDown      = #const KEY_CHANNELDOWN
keyFirst            = #const KEY_FIRST
keyLast             = #const KEY_LAST
keyAB               = #const KEY_AB
keyNext             = #const KEY_NEXT
keyRestart          = #const KEY_RESTART
keySlow             = #const KEY_SLOW
keyShuffle          = #const KEY_SHUFFLE
keyBreak            = #const KEY_BREAK
keyPrevious         = #const KEY_PREVIOUS
keyDigits           = #const KEY_DIGITS
keyTeen             = #const KEY_TEEN
keyTwen             = #const KEY_TWEN
keyVideoPhone       = #const KEY_VIDEOPHONE
keyGames            = #const KEY_GAMES
keyZoomIn           = #const KEY_ZOOMIN
keyZoomOut          = #const KEY_ZOOMOUT
keyZoomReset        = #const KEY_ZOOMRESET
keyWordProcessor    = #const KEY_WORDPROCESSOR
keyEditor           = #const KEY_EDITOR
keySpreadsheet      = #const KEY_SPREADSHEET
keyGraphicsEditor   = #const KEY_GRAPHICSEDITOR
keyPresentation     = #const KEY_PRESENTATION
keyDatabase         = #const KEY_DATABASE
keyNews             = #const KEY_NEWS
keyVoiceMail        = #const KEY_VOICEMAIL
keyAddressBook      = #const KEY_ADDRESSBOOK
keyMessenger        = #const KEY_MESSENGER
keyDisplayToggle    = #const KEY_DISPLAYTOGGLE
keySpellCheck       = #const KEY_SPELLCHECK
keyLogOff           = #const KEY_LOGOFF
keyDollar           = #const KEY_DOLLAR
keyEuro             = #const KEY_EURO
keyFrameBack        = #const KEY_FRAMEBACK
keyFrameForward     = #const KEY_FRAMEFORWARD
keyContactMenu      = #const KEY_CONTEXT_MENU
keyMediaRepeat      = #const KEY_MEDIA_REPEAT
key10ChannelsUp     = #const KEY_10CHANNELSUP
key10ChannelsDown   = #const KEY_10CHANNELSDOWN
keyImages           = #const KEY_IMAGES
keyDelEOL           = #const KEY_DEL_EOL
keyDelEOS           = #const KEY_DEL_EOS
keyInsLine          = #const KEY_INS_LINE
keyDelLine          = #const KEY_DEL_LINE
keyFn               = #const KEY_FN
keyFnEsc            = #const KEY_FN_ESC
keyFnF1             = #const KEY_FN_F1
keyFnF2             = #const KEY_FN_F2
keyFnF3             = #const KEY_FN_F3
keyFnF4             = #const KEY_FN_F4
keyFnF5             = #const KEY_FN_F5
keyFnF6             = #const KEY_FN_F6
keyFnF7             = #const KEY_FN_F7
keyFnF8             = #const KEY_FN_F8
keyFnF9             = #const KEY_FN_F9
keyFnF10            = #const KEY_FN_F10
keyFnF11            = #const KEY_FN_F11
keyFnF12            = #const KEY_FN_F12
keyFn1              = #const KEY_FN_1
keyFn2              = #const KEY_FN_2
keyFnD              = #const KEY_FN_D
keyFnE              = #const KEY_FN_E
keyFnF              = #const KEY_FN_F
keyFnS              = #const KEY_FN_S
keyFnB              = #const KEY_FN_B
keyBrlDot1          = #const KEY_BRL_DOT1
keyBrlDot2          = #const KEY_BRL_DOT2
keyBrlDot3          = #const KEY_BRL_DOT3
keyBrlDot4          = #const KEY_BRL_DOT4
keyBrlDot5          = #const KEY_BRL_DOT5
keyBrlDot6          = #const KEY_BRL_DOT6
keyBrlDot7          = #const KEY_BRL_DOT7
keyBrlDot8          = #const KEY_BRL_DOT8
keyBrlDot9          = #const KEY_BRL_DOT9
keyBrlDot10         = #const KEY_BRL_DOT10
keyNumeric0         = #const KEY_NUMERIC_0
keyNumeric1         = #const KEY_NUMERIC_1
keyNumeric2         = #const KEY_NUMERIC_2
keyNumeric3         = #const KEY_NUMERIC_3
keyNumeric4         = #const KEY_NUMERIC_4
keyNumeric5         = #const KEY_NUMERIC_5
keyNumeric6         = #const KEY_NUMERIC_6
keyNumeric7         = #const KEY_NUMERIC_7
keyNumeric8         = #const KEY_NUMERIC_8
keyNumeric9         = #const KEY_NUMERIC_9
keyNumericStar      = #const KEY_NUMERIC_STAR
keyNumericPound     = #const KEY_NUMERIC_POUND
keyCameraFocus      = #const KEY_CAMERA_FOCUS
keyWPSButton        = #const KEY_WPS_BUTTON
keyTouchpadToggle   = #const KEY_TOUCHPAD_TOGGLE
keyTouchpadOn       = #const KEY_TOUCHPAD_ON
keyTouchpadOff      = #const KEY_TOUCHPAD_OFF
keyCameraZoomIn     = #const KEY_CAMERA_ZOOMIN
keyCameraZoomOut    = #const KEY_CAMERA_ZOOMOUT
keyCameraUp         = #const KEY_CAMERA_UP
keyCameraDown       = #const KEY_CAMERA_DOWN
keyCameraLeft       = #const KEY_CAMERA_LEFT
keyCameraRight      = #const KEY_CAMERA_RIGHT
btnTriggerHappy     = #const BTN_TRIGGER_HAPPY
btnTriggerHappy1    = #const BTN_TRIGGER_HAPPY1
btnTriggerHappy2    = #const BTN_TRIGGER_HAPPY2
btnTriggerHappy3    = #const BTN_TRIGGER_HAPPY3
btnTriggerHappy4    = #const BTN_TRIGGER_HAPPY4
btnTriggerHappy5    = #const BTN_TRIGGER_HAPPY5
btnTriggerHappy6    = #const BTN_TRIGGER_HAPPY6
btnTriggerHappy7    = #const BTN_TRIGGER_HAPPY7
btnTriggerHappy8    = #const BTN_TRIGGER_HAPPY8
btnTriggerHappy9    = #const BTN_TRIGGER_HAPPY9
btnTriggerHappy10   = #const BTN_TRIGGER_HAPPY10
btnTriggerHappy11   = #const BTN_TRIGGER_HAPPY11
btnTriggerHappy12   = #const BTN_TRIGGER_HAPPY12
btnTriggerHappy13   = #const BTN_TRIGGER_HAPPY13
btnTriggerHappy14   = #const BTN_TRIGGER_HAPPY14
btnTriggerHappy15   = #const BTN_TRIGGER_HAPPY15
btnTriggerHappy16   = #const BTN_TRIGGER_HAPPY16
btnTriggerHappy17   = #const BTN_TRIGGER_HAPPY17
btnTriggerHappy18   = #const BTN_TRIGGER_HAPPY18
btnTriggerHappy19   = #const BTN_TRIGGER_HAPPY19
btnTriggerHappy20   = #const BTN_TRIGGER_HAPPY20
btnTriggerHappy21   = #const BTN_TRIGGER_HAPPY21
btnTriggerHappy22   = #const BTN_TRIGGER_HAPPY22
btnTriggerHappy23   = #const BTN_TRIGGER_HAPPY23
btnTriggerHappy24   = #const BTN_TRIGGER_HAPPY24
btnTriggerHappy25   = #const BTN_TRIGGER_HAPPY25
btnTriggerHappy26   = #const BTN_TRIGGER_HAPPY26
btnTriggerHappy27   = #const BTN_TRIGGER_HAPPY27
btnTriggerHappy28   = #const BTN_TRIGGER_HAPPY28
btnTriggerHappy29   = #const BTN_TRIGGER_HAPPY29
btnTriggerHappy30   = #const BTN_TRIGGER_HAPPY30
btnTriggerHappy31   = #const BTN_TRIGGER_HAPPY31
btnTriggerHappy32   = #const BTN_TRIGGER_HAPPY32
btnTriggerHappy33   = #const BTN_TRIGGER_HAPPY33
btnTriggerHappy34   = #const BTN_TRIGGER_HAPPY34
btnTriggerHappy35   = #const BTN_TRIGGER_HAPPY35
btnTriggerHappy36   = #const BTN_TRIGGER_HAPPY36
btnTriggerHappy37   = #const BTN_TRIGGER_HAPPY37
btnTriggerHappy38   = #const BTN_TRIGGER_HAPPY38
btnTriggerHappy39   = #const BTN_TRIGGER_HAPPY39
btnTriggerHappy40   = #const BTN_TRIGGER_HAPPY40

relWheelDown        :: Word16
relWheelUp          :: Word16

relWheelDown        = 0x8000 + #const REL_WHEEL * 2 + 0
relWheelUp          = 0x8000 + #const REL_WHEEL * 2 + 1

{- ########################################################################################## -}
