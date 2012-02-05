{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module F.CommandD.Conn
( Conn(..)
) where
  
{- ########################################################################################## -}
import Control.Monad (forever, mapM_)
import Control.Monad.Trans.State (StateT(..))
import F.CommandD.Daemon
import F.CommandD.Sink
import F.CommandD.Sink.SeqSink
import F.CommandD.Source
{- ########################################################################################## -}

infixr 1 >>>

class Conn a b c | a b -> c where
  (>>>) :: a -> b -> c

instance (SinkC a, SinkC b) => Conn (Sink a) (Sink b) (Sink (SeqSink a b)) where
  (Sink a) >>> (Sink b) = Sink $ SeqSink a b

instance (SourceC a, SinkC b) => Conn (Source a) (Sink b) (CD ()) where
  (Source a) >>> (Sink b) = sourceRun a (\s -> runCE s $ sinkWrite b)
    
instance Conn a b (CD ()) => Conn [a] b (CD ()) where
  a >>> b = mapM_ (>>> b) a
  
{- ########################################################################################## -}
