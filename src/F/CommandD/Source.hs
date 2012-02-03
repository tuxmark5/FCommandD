module F.CommandD.Source
( Source(..)
, SourceC(..)
) where

{- ########################################################################################## -}
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import F.CommandD.Daemon
import F.CommandD.Sink
{- ########################################### F+ ########################################### -}
{- #                                         Source                                         # -}
{- ########################################################################################## -}

class SourceC a where
  sourceRead :: a -> CD Event
  sourceRead _ = return undefined
  
  sourceRun  :: a -> (Event -> CD ()) -> CD ()
  sourceRun s ef = forkCD $ forever $ sourceRead s >>= ef 
  
data SourceC a => Source a = Source a

{- ########################################################################################## -}
