{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module System.Linux.Input
( C'input_id(..)
, Event(..)
, InputId(..)
, Timeval(..)
, absX
, absY
, absZ
, absRX
, absRY
, absRZ
, absThrottle
, absRudder
, absWheel
, absGas
, absBrake
, absHat0X
, absHat0Y
, absHat1X
, absHat1Y
, absHat2X
, absHat2Y
, absHat3X
, absHat3Y
, absPressure
, absDistance
, absTiltX
, absTiltY
, absToolWidth
, absVolume
, absMisc
, absMtSlot
, absMtTouchMajor
, absMtTouchMinor
, absMtWidthMajor
, absMtWidthMinor
, absMtOrientation
, absMtPositionX
, absMtPositionY
, absMtToolType
, absMtBlobId
, absMtTrackingId
, absMtPressure
, absMtDistance
, busPCI
, busIsaPNP
, busUSB
, busHIL
, busBluetooth
, busVirtual
, busISA
, busI8042
, busXTKbd
, busRS232
, busGamePort
, busParPort
, busAmiga
, busADB
, busI2C
, busHost
, busGSC
, busAtaro
, busSPI
, convertInputId
, defaultInputId
, evSYN
, evKEY
, evREL
, evABS
, evMSC
, evSW
, evLED
, evSND
, evREP
, evFF
, evPWR
, evFFStatus
, getTimeOfDay  -- extra
, inputGrab
, inputRead
, inputWrite
, ioctl0        -- extra
, ioctl1        -- extra
, relX
, relY
, relZ
, relRX
, relRY
, relRZ
, relHWheel
, relDial
, relWheel
, relMisc
, storableRead  -- extra
, storableWrite -- extra
) where

{- ########################################################################################## -}
import Data.Binary
import Data.Int
import Data.Word
import Foreign.C.Types (CInt(..), CLong)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable
import System.Posix.IO (fdReadBuf, fdWriteBuf)
import System.Posix.IOCtl
import System.Posix.Types (Fd(..))
{- ########################################################################################## -}
#include <bindings.dsl.h>
#include <linux/input.h>
{- ########################################################################################## -}

#starttype struct input_event
  #field        time,           <timeval>
  #field        type,           Word16
  #field        code,           Word16
  #field        value,          Int32
#stoptype

data Event  = Event
  { eventTime   :: !Timeval
  , eventType   :: !Word16
  , eventCode   :: !Word16
  , eventValue  :: !Int32
  , eventSource :: !Int32
  } deriving (Show)
  
fromEvent :: Event -> C'input_event
fromEvent d = C'input_event
  { c'input_event'time  = fromTimeval $ eventTime d
  , c'input_event'type  = eventType d
  , c'input_event'code  = eventCode d
  , c'input_event'value = eventValue d
  }
  
toEvent :: C'input_event -> Event
toEvent d = Event
  { eventTime   = toTimeval $ c'input_event'time  d
  , eventType   = c'input_event'type  d
  , eventCode   = c'input_event'code  d
  , eventValue  = c'input_event'value d
  , eventSource = 0
  }

{- ########################################################################################## -}

#starttype struct input_id
  #field        bustype,        Word16
  #field        vendor,         Word16
  #field        product,        Word16
  #field        version,        Word16
#stoptype

data InputId = InputId
  { iiBusType         :: Word16
  , iiVendor          :: Word16
  , iiProduct         :: Word16
  , iiVersion         :: Word16
  }
  
convertInputId :: InputId -> C'input_id
convertInputId d      = C'input_id
  { c'input_id'bustype         = iiBusType d
  , c'input_id'vendor          = iiVendor d
  , c'input_id'product         = iiProduct d
  , c'input_id'version         = iiVersion d
  }

defaultInputId :: InputId
defaultInputId = InputId
  { iiBusType         = 0
  , iiVendor          = 0
  , iiProduct         = 0
  , iiVersion         = 0
  }

{- ########################################################################################## -}
  
#starttype struct timeval
  #field        tv_sec,         CLong
  #field        tv_usec,        CLong
#stoptype
  
data Timeval = Timeval
  { timevalSec  :: !Int64
  , timevalUSec :: !Int64
  } deriving (Show)
  
fromTimeval :: Timeval -> C'timeval
fromTimeval d = C'timeval
  { c'timeval'tv_sec  = fromIntegral $ timevalSec d
  , c'timeval'tv_usec = fromIntegral $ timevalUSec d
  }
  
toTimeval :: C'timeval -> Timeval
toTimeval d = Timeval
  { timevalSec        = fromIntegral $ c'timeval'tv_sec d
  , timevalUSec       = fromIntegral $ c'timeval'tv_usec d
  }
  
foreign import ccall "sys/time.h gettimeofday"
  getTimeOfDay' :: Ptr C'timeval -> Ptr () -> IO CInt
                                      
getTimeOfDay :: IO Timeval
getTimeOfDay = alloca $ \p -> do
  getTimeOfDay' p nullPtr 
  peek p >>= return . toTimeval 

{- ########################################################################################## -}
  
data InputGrab = InputGrab
  
instance IOControl InputGrab CInt where ioctlReq _ = #const EVIOCGRAB

{- ########################################################################################## -}
  
absX              :: Int32
absY              :: Int32
absZ              :: Int32
absRX             :: Int32
absRY             :: Int32
absRZ             :: Int32
absThrottle       :: Int32
absRudder         :: Int32
absWheel          :: Int32
absGas            :: Int32
absBrake          :: Int32
absHat0X          :: Int32
absHat0Y          :: Int32
absHat1X          :: Int32
absHat1Y          :: Int32
absHat2X          :: Int32
absHat2Y          :: Int32
absHat3X          :: Int32
absHat3Y          :: Int32
absPressure       :: Int32
absDistance       :: Int32
absTiltX          :: Int32
absTiltY          :: Int32
absToolWidth      :: Int32
absVolume         :: Int32
absMisc           :: Int32
absMtSlot         :: Int32
absMtTouchMajor   :: Int32
absMtTouchMinor   :: Int32
absMtWidthMajor   :: Int32
absMtWidthMinor   :: Int32
absMtOrientation  :: Int32
absMtPositionX    :: Int32
absMtPositionY    :: Int32
absMtToolType     :: Int32
absMtBlobId       :: Int32
absMtTrackingId   :: Int32
absMtPressure     :: Int32
absMtDistance     :: Int32

absX              = #const ABS_X
absY              = #const ABS_Y
absZ              = #const ABS_Z
absRX             = #const ABS_RX
absRY             = #const ABS_RY
absRZ             = #const ABS_RZ
absThrottle       = #const ABS_THROTTLE
absRudder         = #const ABS_RUDDER
absWheel          = #const ABS_WHEEL
absGas            = #const ABS_GAS
absBrake          = #const ABS_BRAKE
absHat0X          = #const ABS_HAT0X
absHat0Y          = #const ABS_HAT0Y
absHat1X          = #const ABS_HAT1X
absHat1Y          = #const ABS_HAT1Y
absHat2X          = #const ABS_HAT2X
absHat2Y          = #const ABS_HAT2Y
absHat3X          = #const ABS_HAT3X
absHat3Y          = #const ABS_HAT3Y
absPressure       = #const ABS_PRESSURE
absDistance       = #const ABS_DISTANCE
absTiltX          = #const ABS_TILT_X
absTiltY          = #const ABS_TILT_Y
absToolWidth      = #const ABS_TOOL_WIDTH
absVolume         = #const ABS_VOLUME
absMisc           = #const ABS_MISC
absMtSlot         = #const ABS_MT_SLOT
absMtTouchMajor   = #const ABS_MT_TOUCH_MAJOR
absMtTouchMinor   = #const ABS_MT_TOUCH_MINOR
absMtWidthMajor   = #const ABS_MT_WIDTH_MAJOR
absMtWidthMinor   = #const ABS_MT_WIDTH_MINOR
absMtOrientation  = #const ABS_MT_ORIENTATION
absMtPositionX    = #const ABS_MT_POSITION_X
absMtPositionY    = #const ABS_MT_POSITION_Y
absMtToolType     = #const ABS_MT_TOOL_TYPE
absMtBlobId       = #const ABS_MT_BLOB_ID
absMtTrackingId   = #const ABS_MT_TRACKING_ID
absMtPressure     = #const ABS_MT_PRESSURE
absMtDistance     = #const ABS_MT_DISTANCE

busPCI            :: Word16
busIsaPNP         :: Word16
busUSB            :: Word16
busHIL            :: Word16
busBluetooth      :: Word16
busVirtual        :: Word16
busISA            :: Word16
busI8042          :: Word16
busXTKbd          :: Word16
busRS232          :: Word16
busGamePort       :: Word16
busParPort        :: Word16
busAmiga          :: Word16
busADB            :: Word16
busI2C            :: Word16
busHost           :: Word16
busGSC            :: Word16
busAtaro          :: Word16
busSPI            :: Word16

busPCI            = #const BUS_PCI         
busIsaPNP         = #const BUS_ISAPNP      
busUSB            = #const BUS_USB         
busHIL            = #const BUS_HIL         
busBluetooth      = #const BUS_BLUETOOTH   
busVirtual        = #const BUS_VIRTUAL     
busISA            = #const BUS_ISA         
busI8042          = #const BUS_I8042       
busXTKbd          = #const BUS_XTKBD       
busRS232          = #const BUS_RS232       
busGamePort       = #const BUS_GAMEPORT    
busParPort        = #const BUS_PARPORT     
busAmiga          = #const BUS_AMIGA       
busADB            = #const BUS_ADB         
busI2C            = #const BUS_I2C         
busHost           = #const BUS_HOST        
busGSC            = #const BUS_GSC         
busAtaro          = #const BUS_ATARI       
busSPI            = #const BUS_SPI

evSYN             :: Word16
evKEY             :: Word16
evREL             :: Word16
evABS             :: Word16
evMSC             :: Word16
evSW              :: Word16
evLED             :: Word16
evSND             :: Word16
evREP             :: Word16
evFF              :: Word16
evPWR             :: Word16
evFFStatus        :: Word16

evSYN             = #const EV_SYN
evKEY             = #const EV_KEY
evREL             = #const EV_REL
evABS             = #const EV_ABS
evMSC             = #const EV_MSC
evSW              = #const EV_SW
evLED             = #const EV_LED
evSND             = #const EV_SND
evREP             = #const EV_REP
evFF              = #const EV_FF
evPWR             = #const EV_PWR
evFFStatus        = #const EV_FF_STATUS

relX              :: Word16
relY              :: Word16
relZ              :: Word16
relRX             :: Word16
relRY             :: Word16
relRZ             :: Word16
relHWheel         :: Word16
relDial           :: Word16
relWheel          :: Word16
relMisc           :: Word16

relX              = #const REL_X
relY              = #const REL_Y
relZ              = #const REL_Z
relRX             = #const REL_RX
relRY             = #const REL_RY
relRZ             = #const REL_RZ
relHWheel         = #const REL_HWHEEL
relDial           = #const REL_DIAL
relWheel          = #const REL_WHEEL
relMisc           = #const REL_MISC

{- ########################################################################################## -}

foreign import ccall "sys/ioctl.h ioctl" 
  ioctl0 :: Fd -> CInt -> IO ()

foreign import ccall "sys/ioctl.h ioctl" 
  ioctl1 :: Fd -> CInt -> CInt -> IO ()
  
--ioctl :: IOControl r d => Fd -> r -> d -> IO ()
--ioctl fd r d = 

inputGrab :: Fd -> Bool -> IO ()
inputGrab fd status = ioctl1 fd (ioctlReq InputGrab) (if status then 1 else 0)

inputRead :: Fd -> IO Event
inputRead fd = storableRead fd >>= return . toEvent

inputWrite :: Fd -> Event -> IO ()
inputWrite fd event = storableWrite fd (fromEvent event)

sizeOfPtr :: Storable a => Ptr a -> a -> Int
sizeOfPtr px x = sizeOf x

storableRead :: Storable a => Fd -> IO a
storableRead fd = alloca $ \ptr -> do
  fdReadBuf fd (castPtr ptr) (fromIntegral $ sizeOfPtr ptr undefined)
  peek ptr
  
storableWrite :: Storable a => Fd -> a -> IO ()
storableWrite fd dat = alloca $ \ptr -> do
  poke ptr dat
  fdWriteBuf fd (castPtr ptr) (fromIntegral $ sizeOf dat)
  return ()
  
{- ########################################################################################## -}
