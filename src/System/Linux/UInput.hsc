{-# LANGUAGE MultiParamTypeClasses #-}

module System.Linux.UInput
( UIControl(..)
, UISetBit(..)
, UIUserDevice(..)
, convertUIUserDevice
, defaultUIUserDevice
, uiCreateDevice
) where

{- ########################################################################################## -}
import Data.Int
import Data.Word
import Foreign.C.String (castCharToCChar)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.Linux.Input
import System.Posix.IO (OpenMode(..), defaultFileFlags, fdWriteBuf, openFd)
import System.Posix.IOCtl
import System.Posix.Types (Fd)
{- ########################################################################################## -}
#include <bindings.dsl.h>
#include <linux/uinput.h>
{- ########################################################################################## -}

#starttype struct uinput_user_dev
  #array_field  name,           CChar
  #field        id,             <input_id>
  #field        ff_effects_max, CInt
  #array_field  absmax,         CInt
  #array_field  absmin,         CInt
  #array_field  absfuzz,        CInt
  #array_field  absflat,        CInt
#stoptype

data UIUserDevice = UIUserDevice
  { uiDevName         :: String
  , uiDevId           :: InputId
  , uiDevFFEffectsMax :: Int
  , uiDevAbsMax       :: [Int]
  , uiDevAbsMin       :: [Int]
  , uiDevAbsFuzz      :: [Int]
  , uiDevAbsFlat      :: [Int]
  }
  
convertUIUserDevice :: UIUserDevice -> C'uinput_user_dev
convertUIUserDevice d = C'uinput_user_dev
  { c'uinput_user_dev'name            = map castCharToCChar $ uiDevName d
  , c'uinput_user_dev'id              = convertInputId (uiDevId d)
  , c'uinput_user_dev'ff_effects_max  = fromIntegral $ uiDevFFEffectsMax d
  , c'uinput_user_dev'absmax          = map fromIntegral $ uiDevAbsMax d
  , c'uinput_user_dev'absmin          = map fromIntegral $ uiDevAbsMin d
  , c'uinput_user_dev'absfuzz         = map fromIntegral $ uiDevAbsFuzz d
  , c'uinput_user_dev'absflat         = map fromIntegral $ uiDevAbsFlat d
  }
  
defaultUIUserDevice :: UIUserDevice
defaultUIUserDevice = UIUserDevice
  { uiDevName         = "HDevice"
  , uiDevId           = defaultInputId
  , uiDevFFEffectsMax = 0
  , uiDevAbsMax       = [0]
  , uiDevAbsMin       = [0]
  , uiDevAbsFuzz      = [0]
  , uiDevAbsFlat      = [0]
  }
  
{- ########################################################################################## -}

instance Storable () where
  alignment _ = 1
  sizeOf    _ = 0

{- ########################################################################################## -}
  
data UIControl 
  = UIDevCreate
  | UIDevDestroy
  
instance IOControl UIControl () where 
  ioctlReq UIDevCreate  = #const UI_DEV_CREATE
  ioctlReq UIDevDestroy = #const UI_DEV_DESTROY
  
{- ########################################################################################## -}
 
data UISetBit 
  = UISetEvBit
  | UISetKeyBit
  | UISetRelBit
  | UISetAbsBit
  | UISetMscBit
  | UISetLedBit
  | UISetSndBit
  | UISetFFBit
  | UISetSWBit
  | UISetPropBit

instance IOControl UISetBit CInt where 
  ioctlReq UISetEvBit   = #const UI_SET_EVBIT
  ioctlReq UISetKeyBit  = #const UI_SET_KEYBIT
  ioctlReq UISetRelBit  = #const UI_SET_RELBIT
  ioctlReq UISetAbsBit  = #const UI_SET_ABSBIT
  ioctlReq UISetMscBit  = #const UI_SET_MSCBIT
  ioctlReq UISetLedBit  = #const UI_SET_LEDBIT
  ioctlReq UISetSndBit  = #const UI_SET_SNDBIT
  ioctlReq UISetFFBit   = #const UI_SET_FFBIT
  ioctlReq UISetSWBit   = #const UI_SET_SWBIT
  ioctlReq UISetPropBit = #const UI_SET_PROPBIT

{- ########################################################################################## -}

uiCreateDevice :: UIUserDevice -> IO Fd
uiCreateDevice d = do
  fd <- openFd "/dev/uinput" WriteOnly Nothing defaultFileFlags
  storableWrite fd (convertUIUserDevice d)
  return fd
  
{- ########################################################################################## -}
