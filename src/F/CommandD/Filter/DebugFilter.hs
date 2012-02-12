module F.CommandD.Filter.DebugFilter
( DebugFilter
, mkDebugFilter
) where

{- ########################################################################################## -}
import F.CommandD.Core
import F.CommandD.Filter
import System.IO
{- ########################################################################################## -}

data DebugFilter = DebugFilter

instance SinkC DebugFilter where
  sinkWrite _ = get >>= \e -> lift $ do
    putStrLn $ show e

mkDebugFilter :: CD (Filter DebugFilter)
mkDebugFilter = return $ Sink $ DebugFilter
  
{- ########################################################################################## -}
