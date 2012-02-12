{-# LANGUAGE MultiParamTypeClasses #-}

module F.CommandD.Sink.UInputSink
( UInputSink
, mkUInputSink
) where

{- ########################################################################################## -}
import Control.Monad (forM_)
import Data.Word (Word16)
import F.CommandD.Core
import F.CommandD.Sink
import System.Linux.Input
import System.Linux.UInput
import System.Posix.IO (OpenMode(..), defaultFileFlags, openFd)
import System.Posix.IOCtl
import System.Posix.Types (Fd)
{- ########################################################################################## -}

data UInputSink = UInputSink
  { uiSinkFd :: Fd
  }

instance SinkC UInputSink where
  sinkWrite s = get >>= \e -> do
    lift $ inputWrite (uiSinkFd s) e

mkUInputSink :: String -> CD (Sink UInputSink)
mkUInputSink name = lift $ do
  fd <- uiCreateDevice defaultUIUserDevice 
    { uiDevName     = name ++ "\0"
    , uiDevId       = defaultInputId
      { iiBusType   = busUSB
      , iiVendor    = 0
      , iiProduct   = 0
      , iiVersion   = 1
      }
    }
    
  forM_ [evSYN, evKEY, evREL]         $ ioctl1 fd (ioctlReq UISetEvBit ) . fromIntegral
  forM_ [1..0x300]                    $ ioctl1 fd (ioctlReq UISetKeyBit) . fromIntegral
  forM_ [relX, relY, relWheel]        $ ioctl1 fd (ioctlReq UISetRelBit) . fromIntegral
  ioctl0 fd (ioctlReq UIDevCreate)
  
  return $ Sink $ UInputSink 
    { uiSinkFd = fd
    }
  
{- ########################################################################################## -}
