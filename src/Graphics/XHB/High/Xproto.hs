{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Graphics.XHB.High.Xproto
( Atom
, Connection(..)
, Event(..)
, EventMask(..)
, Receipt(..)
, SomeEvent(..)
, Window
, getProperty
, internAtom
, selectInput
, waitForEvent
) where
  
{- ########################################################################################## -}  
import            Data.Binary
import            Data.Binary.Get (getByteString, getWord32host, remaining, runGet)
import            Data.Bits
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as B
import qualified  Data.ByteString.Lazy  as BW
import            Data.Word (Word32)
import            Foreign.C.String (castCharToCChar)
import            Foreign.C.Types (CChar)
import            Graphics.XHB (BitEnum(..), Connection, Event(..), Receipt, 
  SimpleEnum(..), SomeEvent(..), XidLike(..), getReply, toMask, toValueParam, waitForEvent)
import            Graphics.XHB.Gen.Xproto (EventMask(..))
import qualified  Graphics.XHB.Gen.Xproto as X
{- ########################################################################################## -}  

type Atom       = X.ATOM
type Window     = X.WINDOW

{- ########################################################################################## -}  

class (Binary p) => Property p where
  propertyGet     :: Get p
  propertyPut     :: p -> Put
  propertySize    :: p -> Word32
  propertyType    :: p -> Atom
  
  propertyGet     = get
  propertyPut  p  = put p
  propertySize _  = 1
  propertyType _  = castEnum X.AtomAny
  
instance Binary X.WINDOW where
  get = getWord32host >>= return . fromXid . toXid
  
instance Property ByteString where
  propertyGet     = remaining >>= getByteString . fromIntegral
  propertySize  _ = 100000
  propertyType  _ = castEnum X.AtomSTRING
  
instance Property X.WINDOW where
  propertyType _  = castEnum X.AtomWINDOW
  
{- ########################################################################################## -}  

castEnum :: (SimpleEnum e, XidLike x) => e -> x
castEnum a = fromXid $ toXid $ ((toValue a) :: Word32)

toCCharList :: ByteString -> [CChar]
toCCharList s = map castCharToCChar (B.unpack s)

{- ########################################################################################## -}  
  
getProperty :: Property p => Connection -> Window -> Atom -> IO (Maybe p)
getProperty conn win atom = getProperty' conn win atom undefined
  
getProperty' :: Property p => Connection -> Window -> Atom ->  p -> IO (Maybe p)
getProperty' conn win atom p' = do
  re <- X.getProperty conn X.MkGetProperty
    { X.delete_GetProperty        = False
    , X.long_length_GetProperty   = propertySize p' 
    , X.long_offset_GetProperty   = 0 
    , X.property_GetProperty      = atom
    , X.type_GetProperty          = propertyType p'
    , X.window_GetProperty        = win
    } >>= getReply
  case re of
    Left  e  -> putStrLn (show e) >> return Nothing
    Right r  -> return $ Just $ runGet propertyGet $ BW.pack $ X.value_GetPropertyReply r
  
internAtom :: Connection -> ByteString -> Bool -> IO (Receipt Atom)
internAtom conn name ex = do
  X.internAtom conn X.MkInternAtom
    { X.name_InternAtom           = toCCharList name
    , X.name_len_InternAtom       = fromIntegral $ B.length name
    , X.only_if_exists_InternAtom = ex
    }

selectInput :: Connection -> Window -> [EventMask] -> IO ()
selectInput conn win mask = X.changeWindowAttributes conn win vp where
  vp = toValueParam [(X.CWEventMask, toMask mask)]

{- ########################################################################################## -}  

