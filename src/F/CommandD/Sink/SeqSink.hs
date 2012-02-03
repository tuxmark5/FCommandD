module F.CommandD.Sink.SeqSink
( SeqSink(..)
--, mkSeqSink
) where

{- ########################################################################################## -}
import F.CommandD.Daemon
import F.CommandD.Sink
{- ########################################################################################## -}

data (SinkC a, SinkC b) => SeqSink a b = SeqSink a b

instance (SinkC a, SinkC b) => SinkC (SeqSink a b) where
  sinkWrite (SeqSink a b) = sinkWrite a >> sinkWrite b

{- ########################################################################################## -}
