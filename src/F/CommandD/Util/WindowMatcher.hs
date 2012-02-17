module F.CommandD.Util.WindowMatcher
( WinM(..)
, mAny
, mClass0
, mClass1
, mName
, runWinM_
) where

{- ########################################################################################## -}
import            Control.Monad
import            Control.Monad.Trans.Class (MonadTrans(..))
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as B
import            F.CommandD.Observer.FocusObserver (FocusEvent(..))
{- ########################################################################################## -}

newtype Monad m => WinM m a = WinM 
  { runWinM :: FocusEvent -> m (Maybe a) }

instance Monad m => Monad (WinM m) where
  return a = WinM $ \_ -> return $ Just a
  m >>= k  = WinM $ \s -> runWinM m s >>= \x -> case x of
    Just r  -> runWinM (k r) s
    Nothing -> return Nothing

instance MonadTrans WinM where
  lift m   = WinM $ \_ -> m >>= return . Just

{- ########################################################################################## -}

asks :: Monad m => (FocusEvent -> a) -> WinM m a
asks proj = WinM $ \e -> return $ Just $ proj e

exit :: Monad m => m () -> WinM m ()
exit m = WinM $ \_ -> m >> return Nothing

{- ########################################################################################## -}

mAny :: Monad m => m () -> WinM m ()
mAny = exit

mClass0 :: Monad m => ByteString -> m () -> WinM m ()
mClass0 c0 m = asks feClass >>= match where
  match (c0':_) = when (c0 == c0') $ exit m
  match _       = return ()

mClass1 :: Monad m => ByteString -> m () -> WinM m ()
mClass1 c1 m = asks feClass >>= match where
  match (_:c1':_) = when (c1 == c1') $ exit m
  match _         = return ()

mName :: Monad m => ByteString -> m () -> WinM m ()
mName name m = asks feName >>= match where
  match name' = when (name == name') $ exit m

runWinM_ :: Monad m => WinM m () -> FocusEvent -> m ()
runWinM_ m e = runWinM m e >>= \_ -> return ()

{- ########################################################################################## -}
