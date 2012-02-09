module F.CommandD.Source
( Source(..)
, SourceC(..)
, SourceId
) where

{- ########################################################################################## -}
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Int (Int32)
import F.CommandD.Core
import F.CommandD.Sink
{- ########################################### F+ ########################################### -}
{- #                                         Source                                         # -}
{- ########################################################################################## -}

type SourceId = Int32

class SourceC a where
  sourceRead :: a -> CD Event
  sourceRead _ = return undefined
  
  sourceRun  :: a -> (Event -> CD ()) -> CD ()
  sourceRun s ef = forkCD $ forever $ catchCD $ sourceRead s >>= ef 
  
data SourceC a => Source a = Source a

{- ########################################################################################## -}
