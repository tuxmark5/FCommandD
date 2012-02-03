module F.CommandD.ContST
( ContST(..)
, exitContST
, get
, mapContST
, put
) where
  
{- ########################################################################################## -}
import Control.Monad (Monad(..))
import Control.Monad.Trans.Class (MonadTrans(..))
{- ########################################################################################## -}

newtype (Monad m) => ContST s r m a = ContST
  { runContST :: s -> (s -> a -> m (s, r)) -> m (s, r)
  }

instance (Monad m) => Monad (ContST s r m) where
  return a = ContST $ \s0 c -> c s0 a
  m >>= k  = ContST $ \s0 c -> runContST m s0 (\s1 a -> runContST (k a) s1 c)

instance MonadTrans (ContST s r) where
  lift m = ContST $ \s0 c -> m >>= c s0

exitContST :: (Monad m) => r -> ContST s r m ()
exitContST r = ContST $ \s0 _ -> return (s0, r)
  
get :: (Monad m) => ContST s r m s
get = ContST $ \s0 c -> c s0 s0
  
--mapContT :: (m r -> m r) -> ContT r m a -> ContT r m a
--mapContT fun m = ContT $ \c -> fun $ runContT m c
  
mapContST :: (Monad m) => (m (s, r) -> m (s, r)) -> ContST s r m a -> ContST s r m a
mapContST fun m = ContST $ \s0 c -> fun $ runContST m s0 c
  
put :: (Monad m) => s -> ContST s r m ()
put s1 = ContST $ \_ c -> c s1 ()
  
{- ########################################################################################## -}
