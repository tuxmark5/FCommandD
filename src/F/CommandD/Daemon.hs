module F.CommandD.Daemon
( module F.CommandD.Filter.DebugFilter
, module F.CommandD.Filter.HubFilter
, module F.CommandD.Filter.MacroFilter
, module F.CommandD.Sink
, module F.CommandD.Sink.UInputSink
, module F.CommandD.Source
, module F.CommandD.Util.Conn
, ByteString
, CD(..)
, CE(..)
, Daemon(..)
, Event(..)
, Word16
, daemon
, forkCD
, lift
, runCE
) where

{- ########################################################################################## -}
import            Control.Monad.Trans.State (evalStateT)
import            Data.ByteString.Char8 (ByteString)
import            Data.Word (Word16)
import            F.CommandD.Core
import            F.CommandD.Filter.DebugFilter
import            F.CommandD.Filter.HubFilter
import            F.CommandD.Filter.MacroFilter
import            F.CommandD.Sink
import            F.CommandD.Sink.UInputSink
import            F.CommandD.Source
import            F.CommandD.Util.Conn
import            System.INotify (INotify, withINotify)
import            System.IO (BufferMode(..), hSetBuffering, stderr, stdout)
{- ########################################################################################## -}
  
daemon :: CD a -> IO a
daemon a = withINotify $ \inotify -> do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  evalStateT a Daemon
    { daeINotify = inotify
    }

{- ########################################################################################## -}
