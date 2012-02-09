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
import            Data.Maybe
import            F.CommandD.Chan
import qualified  Graphics.XHB as X
import            Graphics.XHB.Connection
import            Graphics.XHB.High.Xproto
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
  actW <- getActiveWindow s
  case actW of
    Just w      -> writeFocusEvent s w 
    otherwise   -> return ()
handleEvent _ _ = return ()

handleException :: E.SomeException -> IO ()
handleException e = putStrLn $ "___" ++ show e

mainLoop :: FocusObserver -> IO ()
mainLoop s = do
  evt <- X.waitForEvent $ foConnection s
  case fromEvent evt of
    Just e    -> handleEvent s e >> mainLoop s
    Nothing   -> return ()

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
  cls <- getProp foAtomWMClass
      <| return ""
  cmd <- getProp foAtomWMCommand
      <| return "?"
  nam <- getProp foAtomNetWMName
      <| getProp foAtomWMName
      <| return ""
  writeChan (foChan s) $ FocusEvent cls [cmd] nam w
  where getProp acc = getProperty (foConnection s) w (acc s)
  
{- ########################################################################################## -}
