module F.CommandD.Core
( CD(..)
, CE(..)
, Daemon(..)
, Event(..)
, forkCD
, lift
, runCE
) where
  
{- ########################################################################################## -}
import            Control.Concurrent (forkIO)
import            Control.Monad.Trans.Class (MonadTrans(..))
import            Control.Monad.Trans.State (StateT(..), evalStateT)
import            F.CommandD.ContST
import            System.INotify (INotify)
import            System.Linux.Input (Event(..))
{- ########################################################################################## -}

type CD a = StateT Daemon IO a
type CE a = ContST Event () IO a

data Daemon = Daemon
  { daeINotify    :: INotify
  }

forkCD :: CD () -> CD ()
forkCD d = mapStateT (\x -> forkIO x >> return ()) d

mapStateT :: (Monad m) => (m () -> m b) -> StateT s m () -> StateT s m b
mapStateT fun m = StateT $ \s -> do
  a <- fun $ runStateT m s >> return ()
  return (a, s)

runCE :: Event -> CE () -> CD ()
runCE s0 m = StateT $ \s -> do
  (s2, r2) <- runContST m s0 $ \s1 a1 -> return (s1, ())
  return ((), s)

{- ########################################################################################## -}
