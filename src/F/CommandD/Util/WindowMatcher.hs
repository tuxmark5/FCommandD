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
--import            Control.Monad.Trans.Class
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as B
import            F.CommandD.Observer.FocusObserver(FocusEvent(..))
{- ########################################################################################## -}

newtype WinM a = WinM 
  { runWinM :: FocusEvent -> IO (Maybe a) }

instance Monad WinM where
  return a = WinM $ \_ -> return $ Just a
  m >>= k  = WinM $ \s -> runWinM m s >>= \x -> case x of
    Just r  -> runWinM (k r) s
    Nothing -> return Nothing

--instance MonadTrans WinM where
--  lift m   = WinM $ \_ -> m >>= return . Just

asks :: (FocusEvent -> a) -> WinM a
asks proj = WinM $ \e -> return $ Just $ proj e

exit :: IO () -> WinM ()
exit m = WinM $ \_ -> m >> return Nothing

{- ########################################################################################## -}

mAny :: IO () -> WinM ()
mAny = exit

mClass0 :: ByteString -> IO () -> WinM ()
mClass0 c0 m = asks feClass >>= match where
  match (c0':_) = when (c0 == c0') $ exit m
  match _       = return ()

mClass1 :: ByteString -> IO () -> WinM ()
mClass1 c1 m = asks feClass >>= match where
  match (_:c1':_) = when (c1 == c1') $ exit m
  match _         = return ()

mName :: ByteString -> IO () -> WinM ()
mName name m = asks feName >>= match where
  match name' = when (name == name') $ exit m

runWinM_ :: WinM () -> FocusEvent -> IO ()
runWinM_ m e = runWinM m e >>= \_ -> return ()

{- ########################################################################################## -}
