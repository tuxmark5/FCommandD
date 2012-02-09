module F.CommandD.Daemon
( CD(..)
, CE(..)
, Daemon(..)
, Event(..)
, daemon
, forkCD
, lift
, runCE
) where

{- ########################################################################################## -}
import            Control.Monad.Trans.State (evalStateT)
import            Graphics.X11.Xlib.Misc (initThreads)
import            F.CommandD.Core
import            System.INotify (INotify, withINotify)
import            System.IO (BufferMode(..), hSetBuffering, stderr, stdout)
{- ########################################################################################## -}
  
daemon :: CD a -> IO a
daemon a = withINotify $ \inotify -> do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  initThreads
  evalStateT a Daemon
    { daeINotify = inotify
    }

{- ########################################################################################## -}
