module F.CommandD.Action.Macro
( macro
) where

{- ########################################################################################## -}
import            Control.Monad.Trans.State (gets)
import qualified  Data.ByteString.Char8 as B
import            F.CommandD.Core (lift)
import            F.CommandD.Sink (SinkA(..))
import            F.CommandD.Source.VirtualSource
import            F.CommandD.Util.Commander
{- ########################################################################################## -}

instance CommandC Commander (MacroM ()) where 
  runCommand cmd m = forkIO_ $ withProfile cmd $ \pro -> runCommand (cmd, pro) m
instance CommandC (Commander, Profile) (MacroM ()) where 
  runCommand pro m = runCommand pro $ macro m 

{- ########################################################################################## -}

-- TODO: should dump this into hub and not into current profile's sink
-- TODO: ideally should drop to NEXT of invocation

macro :: MacroM () -> ProM ()
macro m = gets snd >>= \p -> lift $ case prSink p of
  Just (SinkA s)  -> runMacro m s
  Nothing         -> B.putStrLn $ B.concat ["No sink for this profile: ", prName p]

{- ########################################################################################## -}
