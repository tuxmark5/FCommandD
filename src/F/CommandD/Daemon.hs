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
{- ########################################################################################## -}
  
daemon :: CD a -> IO a
daemon a = withINotify $ \inotify -> do
  initThreads
  evalStateT a Daemon
    { daeINotify = inotify
    }

{- ########################################################################################## -}
