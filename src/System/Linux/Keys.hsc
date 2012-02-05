{-# LANGUAGE CPP #-}

module System.Linux.Keys
( btn0
, btn1
, btn2
, btn3
, btn4
, btn5
, btn6
, btn7
, btn8
, btn9
, btnA
, btnB
, btnBack
, btnBase
, btnBase2
, btnBase3
, btnBase4
, btnBase5
, btnBase6
, btnC
, btnDead
, btnDigi
, btnExtra
, btnForward
, btnGamePad
, btnGearDown
, btnGearUp
, btnJoystick
, btnLeft
, btnMiddle
, btnMisc
, btnMode
, btnMouse
, btnPinkie
, btnRight
, btnSelect
, btnSide
, btnStart
, btnStylus
, btnStylus2
, btnTask
, btnThumb
, btnThumb2
, btnThumbL
, btnThumbR
, btnTL
, btnTL2
, btnToolAirbrush
, btnToolBrush
, btnToolDoubleTap
, btnToolFinger
, btnToolLens
, btnToolMouse
, btnToolPen
, btnToolPencil
, btnToolQuadTap
, btnToolRubber
, btnToolTripleTap
, btnTop
, btnTop2
, btnTouch
, btnTR
, btnTR2
, btnTrigger
, btnTriggerHappy
, btnTriggerHappy1
, btnTriggerHappy10
, btnTriggerHappy11
, btnTriggerHappy12
, btnTriggerHappy13
, btnTriggerHappy14
, btnTriggerHappy15
, btnTriggerHappy16
, btnTriggerHappy17
, btnTriggerHappy18
, btnTriggerHappy19
, btnTriggerHappy2
, btnTriggerHappy20
, btnTriggerHappy21
, btnTriggerHappy22
, btnTriggerHappy23
, btnTriggerHappy24
, btnTriggerHappy25
, btnTriggerHappy26
, btnTriggerHappy27
, btnTriggerHappy28
, btnTriggerHappy29
, btnTriggerHappy3
, btnTriggerHappy30
, btnTriggerHappy31
, btnTriggerHappy32
, btnTriggerHappy33
, btnTriggerHappy34
, btnTriggerHappy35
, btnTriggerHappy36
, btnTriggerHappy37
, btnTriggerHappy38
, btnTriggerHappy39
, btnTriggerHappy4
, btnTriggerHappy40
, btnTriggerHappy5
, btnTriggerHappy6
, btnTriggerHappy7
, btnTriggerHappy8
, btnTriggerHappy9
, btnWheel
, btnX
, btnY
, btnZ
, key0
, key1
, key102nd
, key10ChannelsDown
, key10ChannelsUp
, key2
, key3
, key4
, key5
, key6
, key7
, key8
, key9
, keyA
, keyAB
, keyAddressBook
, keyAgain
, keyAlterase
, keyAngle
, keyApostrophe
, keyArchive
, keyAudio
, keyAux
, keyB
, keyBack
, keyBackslash
, keyBackspace
, keyBassBoost
, keyBattery
, keyBlue
, keyBluetooth
, keyBookMarks
, keyBreak
, keyBrightnessDown
, keyBrightnessUp
, keyBrightnessCycle
, keyBrightnessZero
, keyBrlDot1
, keyBrlDot10
, keyBrlDot2
, keyBrlDot3
, keyBrlDot4
, keyBrlDot5
, keyBrlDot6
, keyBrlDot7
, keyBrlDot8
, keyBrlDot9
, keyC
, keyCalc
, keyCalendar
, keyCamera
, keyCameraDown
, keyCameraFocus
, keyCameraLeft
, keyCameraRight
, keyCameraUp
, keyCameraZoomIn
, keyCameraZoomOut
, keyCancel
, keyCapsLock
, keyCD
, keyChannel
, keyChannelDown
, keyChannelUp
, keyChat
, keyClear
, keyClose
, keyCloseCD
, keyCoffee
, keyComma
, keyCompose
, keyComputer
, keyConfig
, keyConnect
, keyContactMenu
, keyCopy
, keyCut
, keyCycleWindows
, keyD
, keyBashboard
, keyDatabase
, keyDelete
, keyDeleteFile
, keyDelEOL
, keyDelEOS
, keyDelLine
, keyDigits
, keyDirection
, keyDirectory
, keyDisplayToggle
, keyDisplayOff
, keyDocuments
, keyDollar
, keyDot
, keyDown
, keyDVD
, keyE
, keyEdit
, keyEditor
, keyEjectCT
, keyEjectCloseCD
, keyEmail
, keyEnd
, keyEnter
, keyEPG
, keyEqual
, keyESC
, keyEuro
, keyExit
, keyF
, keyF1
, keyF10
, keyF11
, keyF12
, keyF13
, keyF14
, keyF15
, keyF16
, keyF17
, keyF18
, keyF19
, keyF2
, keyF20
, keyF21
, keyF22
, keyF23
, keyF24
, keyF3
, keyF4
, keyF5
, keyF6
, keyF7
, keyF8
, keyF9
, keyFastForward
, keyFavorites
, keyFile
, keyFinance
, keyFind
, keyFirst
, keyFn
, keyFn1
, keyFn2
, keyFnB
, keyFnD
, keyFnE
, keyFnEsc
, keyFnF
, keyFnF1
, keyFnF10
, keyFnF11
, keyFnF12
, keyFnF2
, keyFnF3
, keyFnF4
, keyFnF5
, keyFnF6
, keyFnF7
, keyFnF8
, keyFnF9
, keyFnS
, keyForward
, keyForwardMail
, keyFrameBack
, keyFrameForward
, keyFront
, keyG
, keyGames
, keyGoTo
, keyGraphicsEditor
, keyGrave
, keyGreen
, keyH
, keyHangeul
, keyHanguel
, keyHanja
, keyHelp
, keyHenkan
, keyHiragana
, keyHome
, keyHomePage
, keyHP
, keyI
, keyImages
, keyInfo
, keyInsert
, keyInsLine
, keyISO
, keyJ
, keyK
, keyKatakana
, keyKatakanaHiragana
, keyKbdIllumDown
, keyKbdIllumToggle
, keyKbdIllumUp
, keyKeyboard
, keyKp0
, keyKp1
, keyKp2
, keyKp3
, keyKp4
, keyKp5
, keyKp6
, keyKp7
, keyKp8
, keyKp9
, keyKpAsterisk
, keyKpComma
, keyKpDot
, keyKpEnter
, keyKpEqual
, keyKpJpComma
, keyKpLeftParen
, keyKpMInus
, keyKpPlus
, keyKpPlusMinus
, keyKpRightParen
, keyKpSlash
, keyL
, keyLanguage
, keyLast
, keyLeft
, keyLeftAlt
, keyLeftBrace
, keyLeftCtrl
, keyLeftMeta
, keyLeftShift
, keyLineFeed
, keyList
, keyLogOff
, keyM
, keyMacro
, keyMail
, keyMedia
, keyMediaRepeat
, keyMemo
, keyMenu
, keyMessenger
, keyMHP
, keyMinus
, keyMode
, keyMove
, keyMP3
, keyMsDOS
, keyMuhenkan
, keyMute
, keyN
, keyNew
, keyNews
, keyNext
, keyNextSong
, keyNumeric0
, keyNumeric1
, keyNumeric2
, keyNumeric3
, keyNumeric4
, keyNumeric5
, keyNumeric6
, keyNumeric7
, keyNumeric8
, keyNumeric9
, keyNumericPound
, keyNumericStar
, keyNumLock
, keyO
, keyOk
, keyOpen
, keyOption
, keyP
, keyPageDown
, keyPageUp
, keyPaste
, keyPause
, keyPauseCD
, keyPC
, keyPhone
, keyPlay
, keyPlayCD
, keyPlayer
, keyPlayPause
, keyPower
, keyPower2
, keyPresentation
, keyPrevious
, keyPreviousSong
, keyPrint
, keyProg1
, keyProg2
, keyProg3
, keyProg4
, keyProgram
, keyProps
, keyPVR
, keyQ
, keyQuestion
, keyR
, keyRadio
, keyRecord
, keyRed
, keyRedo
, keyRefresh
, keyReply
, keyReserved
, keyRestart
, keyRewind
, keyRFKill
, keyRight
, keyRightAlt
, keyRightBrace
, keyRightCtrl
, keyRightMeta
, keyRightShift
, keyRo
, keyS
, keySAT
, keySAT2
, keySave
, keyScale
, keyScreen
, keyScreenLock
, keyScrollDown
, keyScrollLock
, keyScrollUp
, keySearch
, keySelect
, keySemiColon
, keySend
, keySendFile
, keySetup
, keyShop
, keyShuffle
, keySlash
, keySleep
, keySlow
, keySound
, keySpace
, keySpellCheck
, keySport
, keySpreadsheet
, keyStop
, keyStopCD
, keySubTitle
, keySuspend
, keySwitchVideoMode
, keySysRq
, keyT
, keytab
, keyTape
, keyTeen
, keyText
, keyTime
, keyTitle
, keyTouchpadOff
, keyTouchpadOn
, keyTouchpadToggle
, keyTuner
, keyTV
, keyTV2
, keyTwen
, keyU
, keyUndo
, keyUnknown
, keyUp
, keyUWB
, keyV
, keyVCR
, keyVCR2
, keyVendor
, keyVideo
, keyVideoPhone
, keyVideoNext
, keyVideoPrev
, keyVoiceMail
, keyVolumeDown
, keyVolumeUp
, keyW
, keyWakeUp
, keyWiMax
, keyWLAN
, keyWordProcessor
, keyWPSButton
, keyWWW
, keyX
, keyXFer
, keyY
, keyYellow
, keyYen
, keyZ
, keyZenkakuHankaku
, keyZoom
, keyZoomIn
, keyZoomOut
, keyZoomReset
) where

{- ########################################################################################## -}
import Data.Int (Int32)
{- ########################################################################################## -}
#include <linux/input.h>
{- ########################################################################################## -}

btn0                :: Int32
btn1                :: Int32
btn2                :: Int32
btn3                :: Int32
btn4                :: Int32
btn5                :: Int32
btn6                :: Int32
btn7                :: Int32
btn8                :: Int32
btn9                :: Int32
btnA                :: Int32
btnB                :: Int32
btnBack             :: Int32
btnBase             :: Int32
btnBase2            :: Int32
btnBase3            :: Int32
btnBase4            :: Int32
btnBase5            :: Int32
btnBase6            :: Int32
btnC                :: Int32
btnDead             :: Int32
btnDigi             :: Int32
btnExtra            :: Int32
btnForward          :: Int32
btnGamePad          :: Int32
btnGearDown         :: Int32
btnGearUp           :: Int32
btnJoystick         :: Int32
btnLeft             :: Int32
btnMiddle           :: Int32
btnMisc             :: Int32
btnMode             :: Int32
btnMouse            :: Int32
btnPinkie           :: Int32
btnRight            :: Int32
btnSelect           :: Int32
btnSide             :: Int32
btnStart            :: Int32
btnStylus           :: Int32
btnStylus2          :: Int32
btnTask             :: Int32
btnThumb            :: Int32
btnThumb2           :: Int32
btnThumbL           :: Int32
btnThumbR           :: Int32
btnTL               :: Int32
btnTL2              :: Int32
btnToolAirbrush     :: Int32
btnToolBrush        :: Int32
btnToolDoubleTap    :: Int32
btnToolFinger       :: Int32
btnToolLens         :: Int32
btnToolMouse        :: Int32
btnToolPen          :: Int32
btnToolPencil       :: Int32
btnToolQuadTap      :: Int32
btnToolRubber       :: Int32
btnToolTripleTap    :: Int32
btnTop              :: Int32
btnTop2             :: Int32
btnTouch            :: Int32
btnTR               :: Int32
btnTR2              :: Int32
btnTrigger          :: Int32
btnTriggerHappy     :: Int32
btnTriggerHappy1    :: Int32
btnTriggerHappy10   :: Int32
btnTriggerHappy11   :: Int32
btnTriggerHappy12   :: Int32
btnTriggerHappy13   :: Int32
btnTriggerHappy14   :: Int32
btnTriggerHappy15   :: Int32
btnTriggerHappy16   :: Int32
btnTriggerHappy17   :: Int32
btnTriggerHappy18   :: Int32
btnTriggerHappy19   :: Int32
btnTriggerHappy2    :: Int32
btnTriggerHappy20   :: Int32
btnTriggerHappy21   :: Int32
btnTriggerHappy22   :: Int32
btnTriggerHappy23   :: Int32
btnTriggerHappy24   :: Int32
btnTriggerHappy25   :: Int32
btnTriggerHappy26   :: Int32
btnTriggerHappy27   :: Int32
btnTriggerHappy28   :: Int32
btnTriggerHappy29   :: Int32
btnTriggerHappy3    :: Int32
btnTriggerHappy30   :: Int32
btnTriggerHappy31   :: Int32
btnTriggerHappy32   :: Int32
btnTriggerHappy33   :: Int32
btnTriggerHappy34   :: Int32
btnTriggerHappy35   :: Int32
btnTriggerHappy36   :: Int32
btnTriggerHappy37   :: Int32
btnTriggerHappy38   :: Int32
btnTriggerHappy39   :: Int32
btnTriggerHappy4    :: Int32
btnTriggerHappy40   :: Int32
btnTriggerHappy5    :: Int32
btnTriggerHappy6    :: Int32
btnTriggerHappy7    :: Int32
btnTriggerHappy8    :: Int32
btnTriggerHappy9    :: Int32
btnWheel            :: Int32
btnX                :: Int32
btnY                :: Int32
btnZ                :: Int32

{- ########################################################################################## -}

key0                :: Int32
key1                :: Int32
key102nd            :: Int32
key10ChannelsDown   :: Int32
key10ChannelsUp     :: Int32
key2                :: Int32
key3                :: Int32
key4                :: Int32
key5                :: Int32
key6                :: Int32
key7                :: Int32
key8                :: Int32
key9                :: Int32
keyA                :: Int32
keyAB               :: Int32
keyAddressBook      :: Int32
keyAgain            :: Int32
keyAlterase         :: Int32
keyAngle            :: Int32
keyApostrophe       :: Int32
keyArchive          :: Int32
keyAudio            :: Int32
keyAux              :: Int32
keyB                :: Int32
keyBack             :: Int32
keyBackslash        :: Int32
keyBackspace        :: Int32
keyBassBoost        :: Int32
keyBattery          :: Int32
keyBlue             :: Int32
keyBluetooth        :: Int32
keyBookMarks        :: Int32
keyBreak            :: Int32
keyBrightnessDown   :: Int32
keyBrightnessUp     :: Int32
keyBrightnessCycle  :: Int32
keyBrightnessZero   :: Int32
keyBrlDot1          :: Int32
keyBrlDot10         :: Int32
keyBrlDot2          :: Int32
keyBrlDot3          :: Int32
keyBrlDot4          :: Int32
keyBrlDot5          :: Int32
keyBrlDot6          :: Int32
keyBrlDot7          :: Int32
keyBrlDot8          :: Int32
keyBrlDot9          :: Int32
keyC                :: Int32
keyCalc             :: Int32
keyCalendar         :: Int32
keyCamera           :: Int32
keyCameraDown       :: Int32
keyCameraFocus      :: Int32
keyCameraLeft       :: Int32
keyCameraRight      :: Int32
keyCameraUp         :: Int32
keyCameraZoomIn     :: Int32
keyCameraZoomOut    :: Int32
keyCancel           :: Int32
keyCapsLock         :: Int32
keyCD               :: Int32
keyChannel          :: Int32
keyChannelDown      :: Int32
keyChannelUp        :: Int32
keyChat             :: Int32
keyClear            :: Int32
keyClose            :: Int32
keyCloseCD          :: Int32
keyCoffee           :: Int32
keyComma            :: Int32
keyCompose          :: Int32
keyComputer         :: Int32
keyConfig           :: Int32
keyConnect          :: Int32
keyContactMenu      :: Int32
keyCopy             :: Int32
keyCut              :: Int32
keyCycleWindows     :: Int32
keyD                :: Int32
keyBashboard        :: Int32
keyDatabase         :: Int32
keyDelete           :: Int32
keyDeleteFile       :: Int32
keyDelEOL           :: Int32
keyDelEOS           :: Int32
keyDelLine          :: Int32
keyDigits           :: Int32
keyDirection        :: Int32
keyDirectory        :: Int32
keyDisplayToggle    :: Int32
keyDisplayOff       :: Int32
keyDocuments        :: Int32
keyDollar           :: Int32
keyDot              :: Int32
keyDown             :: Int32
keyDVD              :: Int32
keyE                :: Int32
keyEdit             :: Int32
keyEditor           :: Int32
keyEjectCT          :: Int32
keyEjectCloseCD     :: Int32
keyEmail            :: Int32
keyEnd              :: Int32
keyEnter            :: Int32
keyEPG              :: Int32
keyEqual            :: Int32
keyESC              :: Int32
keyEuro             :: Int32
keyExit             :: Int32
keyF                :: Int32
keyF1               :: Int32
keyF10              :: Int32
keyF11              :: Int32
keyF12              :: Int32
keyF13              :: Int32
keyF14              :: Int32
keyF15              :: Int32
keyF16              :: Int32
keyF17              :: Int32
keyF18              :: Int32
keyF19              :: Int32
keyF2               :: Int32
keyF20              :: Int32
keyF21              :: Int32
keyF22              :: Int32
keyF23              :: Int32
keyF24              :: Int32
keyF3               :: Int32
keyF4               :: Int32
keyF5               :: Int32
keyF6               :: Int32
keyF7               :: Int32
keyF8               :: Int32
keyF9               :: Int32
keyFastForward      :: Int32
keyFavorites        :: Int32
keyFile             :: Int32
keyFinance          :: Int32
keyFind             :: Int32
keyFirst            :: Int32
keyFn               :: Int32
keyFn1              :: Int32
keyFn2              :: Int32
keyFnB              :: Int32
keyFnD              :: Int32
keyFnE              :: Int32
keyFnEsc            :: Int32
keyFnF              :: Int32
keyFnF1             :: Int32
keyFnF10            :: Int32
keyFnF11            :: Int32
keyFnF12            :: Int32
keyFnF2             :: Int32
keyFnF3             :: Int32
keyFnF4             :: Int32
keyFnF5             :: Int32
keyFnF6             :: Int32
keyFnF7             :: Int32
keyFnF8             :: Int32
keyFnF9             :: Int32
keyFnS              :: Int32
keyForward          :: Int32
keyForwardMail      :: Int32
keyFrameBack        :: Int32
keyFrameForward     :: Int32
keyFront            :: Int32
keyG                :: Int32
keyGames            :: Int32
keyGoTo             :: Int32
keyGraphicsEditor   :: Int32
keyGrave            :: Int32
keyGreen            :: Int32
keyH                :: Int32
keyHangeul          :: Int32
keyHanguel          :: Int32
keyHanja            :: Int32
keyHelp             :: Int32
keyHenkan           :: Int32
keyHiragana         :: Int32
keyHome             :: Int32
keyHomePage         :: Int32
keyHP               :: Int32
keyI                :: Int32
keyImages           :: Int32
keyInfo             :: Int32
keyInsert           :: Int32
keyInsLine          :: Int32
keyISO              :: Int32
keyJ                :: Int32
keyK                :: Int32
keyKatakana         :: Int32
keyKatakanaHiragana :: Int32
keyKbdIllumDown     :: Int32
keyKbdIllumToggle   :: Int32
keyKbdIllumUp       :: Int32
keyKeyboard         :: Int32
keyKp0              :: Int32
keyKp1              :: Int32
keyKp2              :: Int32
keyKp3              :: Int32
keyKp4              :: Int32
keyKp5              :: Int32
keyKp6              :: Int32
keyKp7              :: Int32
keyKp8              :: Int32
keyKp9              :: Int32
keyKpAsterisk       :: Int32
keyKpComma          :: Int32
keyKpDot            :: Int32
keyKpEnter          :: Int32
keyKpEqual          :: Int32
keyKpJpComma        :: Int32
keyKpLeftParen      :: Int32
keyKpMInus          :: Int32
keyKpPlus           :: Int32
keyKpPlusMinus      :: Int32
keyKpRightParen     :: Int32
keyKpSlash          :: Int32
keyL                :: Int32
keyLanguage         :: Int32
keyLast             :: Int32
keyLeft             :: Int32
keyLeftAlt          :: Int32
keyLeftBrace        :: Int32
keyLeftCtrl         :: Int32
keyLeftMeta         :: Int32
keyLeftShift        :: Int32
keyLineFeed         :: Int32
keyList             :: Int32
keyLogOff           :: Int32
keyM                :: Int32
keyMacro            :: Int32
keyMail             :: Int32
keyMedia            :: Int32
keyMediaRepeat      :: Int32
keyMemo             :: Int32
keyMenu             :: Int32
keyMessenger        :: Int32
keyMHP              :: Int32
keyMinus            :: Int32
keyMode             :: Int32
keyMove             :: Int32
keyMP3              :: Int32
keyMsDOS            :: Int32
keyMuhenkan         :: Int32
keyMute             :: Int32
keyN                :: Int32
keyNew              :: Int32
keyNews             :: Int32
keyNext             :: Int32
keyNextSong         :: Int32
keyNumeric0         :: Int32
keyNumeric1         :: Int32
keyNumeric2         :: Int32
keyNumeric3         :: Int32
keyNumeric4         :: Int32
keyNumeric5         :: Int32
keyNumeric6         :: Int32
keyNumeric7         :: Int32
keyNumeric8         :: Int32
keyNumeric9         :: Int32
keyNumericPound     :: Int32
keyNumericStar      :: Int32
keyNumLock          :: Int32
keyO                :: Int32
keyOk               :: Int32
keyOpen             :: Int32
keyOption           :: Int32
keyP                :: Int32
keyPageDown         :: Int32
keyPageUp           :: Int32
keyPaste            :: Int32
keyPause            :: Int32
keyPauseCD          :: Int32
keyPC               :: Int32
keyPhone            :: Int32
keyPlay             :: Int32
keyPlayCD           :: Int32
keyPlayer           :: Int32
keyPlayPause        :: Int32
keyPower            :: Int32
keyPower2           :: Int32
keyPresentation     :: Int32
keyPrevious         :: Int32
keyPreviousSong     :: Int32
keyPrint            :: Int32
keyProg1            :: Int32
keyProg2            :: Int32
keyProg3            :: Int32
keyProg4            :: Int32
keyProgram          :: Int32
keyProps            :: Int32
keyPVR              :: Int32
keyQ                :: Int32
keyQuestion         :: Int32
keyR                :: Int32
keyRadio            :: Int32
keyRecord           :: Int32
keyRed              :: Int32
keyRedo             :: Int32
keyRefresh          :: Int32
keyReply            :: Int32
keyReserved         :: Int32
keyRestart          :: Int32
keyRewind           :: Int32
keyRFKill           :: Int32
keyRight            :: Int32
keyRightAlt         :: Int32
keyRightBrace       :: Int32
keyRightCtrl        :: Int32
keyRightMeta        :: Int32
keyRightShift       :: Int32
keyRo               :: Int32
keyS                :: Int32
keySAT              :: Int32
keySAT2             :: Int32
keySave             :: Int32
keyScale            :: Int32
keyScreen           :: Int32
keyScreenLock       :: Int32
keyScrollDown       :: Int32
keyScrollLock       :: Int32
keyScrollUp         :: Int32
keySearch           :: Int32
keySelect           :: Int32
keySemiColon        :: Int32
keySend             :: Int32
keySendFile         :: Int32
keySetup            :: Int32
keyShop             :: Int32
keyShuffle          :: Int32
keySlash            :: Int32
keySleep            :: Int32
keySlow             :: Int32
keySound            :: Int32
keySpace            :: Int32
keySpellCheck       :: Int32
keySport            :: Int32
keySpreadsheet      :: Int32
keyStop             :: Int32
keyStopCD           :: Int32
keySubTitle         :: Int32
keySuspend          :: Int32
keySwitchVideoMode  :: Int32
keySysRq            :: Int32
keyT                :: Int32
keytab              :: Int32
keyTape             :: Int32
keyTeen             :: Int32
keyText             :: Int32
keyTime             :: Int32
keyTitle            :: Int32
keyTouchpadOff      :: Int32
keyTouchpadOn       :: Int32
keyTouchpadToggle   :: Int32
keyTuner            :: Int32
keyTV               :: Int32
keyTV2              :: Int32
keyTwen             :: Int32
keyU                :: Int32
keyUndo             :: Int32
keyUnknown          :: Int32
keyUp               :: Int32
keyUWB              :: Int32
keyV                :: Int32
keyVCR              :: Int32
keyVCR2             :: Int32
keyVendor           :: Int32
keyVideo            :: Int32
keyVideoPhone       :: Int32
keyVideoNext        :: Int32
keyVideoPrev        :: Int32
keyVoiceMail        :: Int32
keyVolumeDown       :: Int32
keyVolumeUp         :: Int32
keyW                :: Int32
keyWakeUp           :: Int32
keyWiMax            :: Int32
keyWLAN             :: Int32
keyWordProcessor    :: Int32
keyWPSButton        :: Int32
keyWWW              :: Int32
keyX                :: Int32
keyXFer             :: Int32
keyY                :: Int32
keyYellow           :: Int32
keyYen              :: Int32
keyZ                :: Int32
keyZenkakuHankaku   :: Int32
keyZoom             :: Int32
keyZoomIn           :: Int32
keyZoomOut          :: Int32
keyZoomReset        :: Int32

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

{- ########################################################################################## -}
