module F.CommandD.Action.Mode
( enableSessionMode
, modeSwitcher
, setMode
, setModeLSX
, setModeSX
, toggleModes
) where

{- ########################################################################################## -}
import            Control.Monad (forM_)
import            Control.Monad.Trans.State (StateT, evalStateT, get, gets)
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as B
import            Data.IORef (modifyIORef, writeIORef)
import            F.CommandD.Core hiding (get)
import            F.CommandD.Filter.MacroFilter
import            F.CommandD.Observer.FocusObserver (FocusEvent(..))
import            F.CommandD.Util.Commander
import            F.CommandD.Util.WindowMatcher (WinM(..), runWinM_)
{- ########################################################################################## -}

{- ########################################################################################## -}

enableSessionMode :: ProM ()
enableSessionMode = setModeSX

modeSwitcher :: WinM (StateT Commander IO) () -> FocusEvent -> CmdM ()
modeSwitcher m e = runWinM_ m e

setMode :: [ByteString] -> Bool -> CmdM ()
setMode path e = withMacroMode $ \root -> do
    withMode root path $ setMode' e

-- Local Session eXclusive
setModeLSX :: ByteString -> CmdM ()
setModeLSX name = do
  pro <- get >>= \cmd -> lift (getFirstProfile cmd) >>= return . prName
  withMacroMode $ \root -> do
    withMode root ["local"]          $ setModeX' name
    withMode root ["session", pro]   $ setModeX' name

-- Session eXclusive
setModeSX :: ProM ()
setModeSX = do
  cmd   <- gets (fst)
  name  <- gets (prName . snd)
  lift $ withRootMode (cmdMacroFilt cmd) $ \root -> do
    withMode root ["session"] $ setModeX' name

toggleModes :: [[ByteString]] -> CmdM ()
toggleModes modes = withMacroMode $ \root -> do
  forM_ modes $ \m ->  withMode root m $ toggleMode'

{- ########################################################################################## -}

setMode' :: Bool -> Mode -> IO ()
setMode' e mode = writeIORef (modeEnabled mode) e

setModeX' :: ByteString -> Mode -> IO ()
setModeX' name mode = do
  forM_ (modeChildren mode) $ \m -> do
    setMode' (modeName m == name) m

toggleMode' :: Mode -> IO ()
toggleMode' mode = modifyIORef (modeEnabled mode) $ \e -> not e

withMacroMode :: (Mode -> IO ()) -> CmdM ()
withMacroMode m = gets cmdMacroFilt >>= \macro -> lift $ withRootMode macro m

{- ########################################################################################## -}
