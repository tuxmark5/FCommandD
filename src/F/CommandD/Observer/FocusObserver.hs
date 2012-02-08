{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module F.CommandD.Observer.FocusObserver
( FocusEvent(..)
, FocusObserver(..)
, newFocusObserver
) where
  
{- ########################################################################################## -}
import            Control.Concurrent (forkIO)
import qualified  Control.Exception as E
import            Control.Monad (forever, forM_, when)
import            Control.Monad.Trans.Reader 
import            Data.Bits
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as B
import            Data.IORef
import            F.CommandD.Chan
import            Graphics.X11.Types
import            Graphics.X11.Xlib
import            Graphics.X11.Xlib.Extras
import            System.IO
{- ########################################################################################## -}

data FocusEvent = FocusEvent
  { feClass                 :: ByteString
  , feCommand               :: [ByteString]
  , feName                  :: ByteString
  , feWindow                :: Window
  } deriving (Show)

data FocusObserver = FocusObserver
  { foActiveWindow          :: IORef Window
  , foAtomNetActiveWindow   :: Atom
  , foAtomNetWMName         :: Atom
  , foAtomWMClass           :: Atom
  , foAtomWMCommand         :: Atom
  , foAtomWMName            :: Atom
  , foChan                  :: ChanO FocusEvent
  , foDisplay               :: Display
  , foRootWindow            :: Window
  }
 
{- ########################################################################################## -}

infixr 1 <|

(<|) :: IO a -> IO a -> IO a
a <| b = E.catch a $ \(E.SomeException _) -> b

getActiveWindow :: FocusObserver -> IO (Maybe [Window])
getActiveWindow s = rawGetWindowProperty 32 (foDisplay s) (foAtomNetActiveWindow s) (foRootWindow s) 

getTextProperty' :: Display -> Window -> Atom -> IO ByteString
getTextProperty' _ 0 _ = return ""
getTextProperty' d w a = do
  tp  <- getTextProperty d w a
  str <- B.packCString $ tp_value tp
  xFree $ tp_value tp
  return str

handleEvent :: FocusObserver -> Event -> IO ()
handleEvent s e@PropertyEvent { } | ev_atom e == foAtomNetActiveWindow s = do
  actW <- getActiveWindow s
  case actW of
    Just (w:_)  -> writeFocusEvent s w 
    otherwise   -> return ()
handleEvent _ _ = return ()

handleException :: E.SomeException -> IO ()
handleException e = putStrLn $ "___" ++ show e

mainLoop :: FocusObserver -> IO ()
mainLoop s = allocaXEvent $ \evt -> forever $ do
  nextEvent (foDisplay s) evt
  getEvent evt >>= E.handle handleException . handleEvent s

newFocusObserver :: ByteString -> IO (FocusObserver, ChanI FocusEvent)
newFocusObserver dpy' = do
  dpy       <- openDisplay $ B.unpack dpy'
  (cI, cO)  <- newChan
  rootW     <- rootWindow dpy (defaultScreen dpy)
  atNAW     <- internAtom dpy "_NET_ACTIVE_WINDOW"  False
  atNWN     <- internAtom dpy "_NET_WM_NAME"        False
  atWCl     <- internAtom dpy "WM_CLASS"            False
  atWCm     <- internAtom dpy "WM_COMMAND"          False
  atWNa     <- internAtom dpy "WM_NAME"             False
  aw        <- newIORef 0
  
  selectInput dpy rootW $ propertyChangeMask
  let fo = FocusObserver { 
      foActiveWindow        = aw
    , foAtomNetActiveWindow = atNAW
    , foAtomNetWMName       = atNWN
    , foAtomWMClass         = atWCl
    , foAtomWMCommand       = atWCm
    , foAtomWMName          = atWNa
    , foChan                = cO
    , foDisplay             = dpy
    , foRootWindow          = rootW
    }
    
  forkIO $ mainLoop fo
  return (fo, cI)

writeFocusEvent :: FocusObserver -> Window -> IO ()
writeFocusEvent s w = do
  cls <- getProp foAtomWMClass
      <| return ""
  cmd <- getProp foAtomWMCommand
      <| return "?"
  nam <- getProp foAtomNetWMName
      <| getProp foAtomWMName
      <| return ""
  writeChan (foChan s) $ FocusEvent cls [cmd] nam w
  where getProp acc = getTextProperty' (foDisplay s) w (acc s) 
  
{- ########################################################################################## -}
