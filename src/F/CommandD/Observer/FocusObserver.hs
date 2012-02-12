{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module F.CommandD.Observer.FocusObserver
( FocusEvent(..)
, FocusObserver(..)
, newFocusEvent
, newFocusObserver
) where
  
{- ########################################################################################## -}
import            Control.Concurrent (forkIO)
import qualified  Control.Exception as E
import            Control.Monad (forever, forM_, when)
import            Control.Monad.Trans.Reader 
import            Data.Bits
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString as BW
import qualified  Data.ByteString.Char8 as B
import            Data.IORef
import            Data.Maybe
import            Data.Word (Word32)
import            F.CommandD.Util.Chan
import qualified  Graphics.XHB as X
import            Graphics.XHB.Connection
import            Graphics.XHB.High.Xproto
import            System.IO
{- ########################################################################################## -}

data FocusEvent 
  = FocusChanged
  { feClass                 :: [ByteString]
  , feCommand               :: [ByteString]
  , feName                  :: ByteString
  , feWindow                :: Window
  } 
  | FocusDestroyed
  deriving (Show)

data FocusObserver = FocusObserver
  { foActiveWindow          :: IORef Window 
  , foAtomNetActiveWindow   :: Atom
  , foAtomNetWMName         :: Atom
  , foAtomWMClass           :: Atom
  , foAtomWMCommand         :: Atom
  , foAtomWMName            :: Atom
  , foChan                  :: ChanO FocusEvent
  , foConnection            :: Connection
  , foRootWindow            :: Window
  }
 
{- ########################################################################################## -}

infixr 1 <|

(<|) :: IO (Maybe a) -> IO a -> IO a
a <| b = a >>= \ar -> case ar of
  Just r  -> return r
  Nothing -> b

g :: IO (Receipt a) -> IO a
g r = do
  (Right a) <- r >>= X.getReply
  return a

getActiveWindow :: FocusObserver -> IO (Maybe Window)
getActiveWindow s = getProperty (foConnection s) (foRootWindow s) (foAtomNetActiveWindow s)

handleEvent :: FocusObserver -> X.PropertyNotifyEvent -> IO ()
handleEvent s e | X.atom_PropertyNotifyEvent e == foAtomNetActiveWindow s = do
  actW0  <- readIORef $ foActiveWindow s 
  actW1  <- getActiveWindow s
  case actW1 of
    Just w      -> when (w /= actW0) $ writeFocusEvent s w 
    otherwise   -> return ()
handleEvent _ _ = return ()

handleException :: E.SomeException -> IO ()
handleException e = putStrLn $ "___" ++ show e

mainLoop :: FocusObserver -> IO ()
mainLoop s = do
  err <- X.pollForError $ foConnection s
  evt <- X.waitForEvent $ foConnection s
  case fromEvent evt of
    Just e    -> handleEvent s e 
    Nothing   -> return ()
  case err of
    Just e    -> putStrLn ">> ERROR <<"
    Nothing   -> mainLoop s

newFocusEvent :: FocusEvent
newFocusEvent = FocusChanged
  { feClass   = []
  , feCommand = []
  , feName    = ""
  , feWindow  = X.fromXid $ X.toXid (0 :: Word32)
  }

newFocusObserver :: ByteString -> IO (FocusObserver, ChanI FocusEvent)
newFocusObserver dpy' = do
  (Just conn) <- connectTo $ B.unpack dpy'
  (cI, cO)    <- newChan
  rootW       <- return $ getRoot conn 
  atNAW       <- g $ internAtom conn "_NET_ACTIVE_WINDOW"  False
  atNWN       <- g $ internAtom conn "_NET_WM_NAME"        False
  atWCl       <- g $ internAtom conn "WM_CLASS"            False
  atWCm       <- g $ internAtom conn "WM_COMMAND"          False
  atWNa       <- g $ internAtom conn "WM_NAME"             False
  aw          <- newIORef rootW
  
  selectInput conn rootW [EventMaskPropertyChange]
  let fo = FocusObserver { 
      foActiveWindow        = aw
    , foAtomNetActiveWindow = atNAW
    , foAtomNetWMName       = atNWN
    , foAtomWMClass         = atWCl
    , foAtomWMCommand       = atWCm
    , foAtomWMName          = atWNa
    , foChan                = cO
    , foConnection          = conn
    , foRootWindow          = rootW
    }
    
  forkIO $ mainLoop fo
  return (fo, cI)

writeFocusEvent :: FocusObserver -> Window -> IO ()
writeFocusEvent s w = do
  writeIORef (foActiveWindow s) w
  cls <- getProp foAtomWMClass
      <| return ""
  cmd <- getProp foAtomWMCommand
      <| return ""
  nam <- getProp foAtomWMName
      <| getProp foAtomNetWMName
      <| return ""
  writeChan (foChan s) $ FocusChanged 
    { feClass   = splitString cls 
    , feCommand = splitString cmd 
    , feName    = nam 
    , feWindow  = w
    } 
  where getProp acc = getProperty (foConnection s) w (acc s)

 {- ########################################################################################## -}

splitString :: ByteString -> [ByteString]
splitString s | B.null s  = []
              | otherwise = a:(splitString $ B.drop 1 rest)
  where (a, rest) = BW.breakByte 0 s

{- ########################################################################################## -}
