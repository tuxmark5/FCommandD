module F.CommandD.Sink.NullSink
( NullSink
, mkNullSink
) where

{- ########################################################################################## -}
import F.CommandD.Daemon
import F.CommandD.Sink
{- ########################################################################################## -}

data NullSink = NullSink

instance SinkC NullSink where
  sinkWrite _ _ = return True

mkNullSink :: CD (Sink NullSink)
mkNullSink = return $ Sink $ NullSink
  
{- ########################################################################################## -}
