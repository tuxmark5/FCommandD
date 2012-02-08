module F.CommandD.Action.DBus
( BusName
, InterfaceName
, IsVariant(..)
, MemberName
, MethodCall
, ObjectPath
, Variant
, newCall
) where
  
{- ########################################################################################## -}
import qualified  Data.Set as S
import            DBus.Client
import            DBus.Message (Flag(..), MethodCall(..))
import            DBus.Types
{- ########################################################################################## -}

newCall :: BusName -> ObjectPath -> InterfaceName -> MemberName -> [Variant] -> MethodCall
newCall dst p i m a = MethodCall
  { methodCallPath        = p
  , methodCallMember      = m
  , methodCallInterface   = Just i
  , methodCallDestination = Just dst
  , methodCallFlags       = S.fromList [NoReplyExpected]
  , methodCallBody        = a
  }

{- ########################################################################################## -}
