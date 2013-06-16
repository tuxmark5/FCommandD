module F.CommandD.Action.DBus
( BusName
, InterfaceName
, IsVariant(..)
, MemberName
, MethodCall
, ObjectPath
, Variant
, call
, newCall
) where
  
{- ########################################################################################## -}
import            Control.Monad.Trans.State (gets)
import qualified  Data.ByteString.Char8 as B
import qualified  Data.Set as S
import            DBus hiding (call)
import            DBus.Client (call_)
import            F.CommandD.Core (lift)
import            F.CommandD.Util.Commander
{- ########################################################################################## -}

instance CommandC Commander MethodCall where 
  runCommand cmd c = forkIO_ $ withProfile cmd $ \pro -> runCommand (cmd, pro) c
instance CommandC (Commander, Profile) MethodCall where 
  runCommand pro c = runCommand pro $ call c

{- ########################################################################################## -}

call :: MethodCall -> ProM ()
call c = gets snd >>= \p -> lift $ do
  case prClient p of
    Just client   -> call_ client c >> return ()
    Nothing       -> B.putStrLn $ B.concat ["No client for this profile: ", prName p]

newCall :: BusName -> ObjectPath -> InterfaceName -> MemberName -> [Variant] -> MethodCall
newCall dst p i m a = (methodCall p i m)
  { methodCallDestination = Just dst
  , methodCallBody        = a
  }

{- ########################################################################################## -}
