{-# LANGUAGE ExistentialQuantification #-}

module F.CommandD.Sink
( Sink(..)
, SinkA(..)
, SinkC(..)
) where

{- ########################################################################################## -}
import F.CommandD.Daemon
{- ########################################################################################## -}

class SinkC a where
  sinkWrite :: a -> CE ()

data SinkC a => Sink a  = Sink a

data SinkA = forall a . SinkC a => SinkA a

{- ########################################################################################## -}
