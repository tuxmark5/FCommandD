module F.CommandD.Filter.DebugFilter
( DebugFilter
, mkDebugFilter
) where

{- ########################################################################################## -}
import F.CommandD.Daemon
import F.CommandD.Filter
{- ########################################################################################## -}

data DebugFilter = DebugFilter

instance FilterC DebugFilter where
  sinkWrite _ = lift $ do
    putStrLn "GOT IT"

mkDebugFilter :: CD Filter
mkDebugFilter = return $ Filter $ DebugFilter
  
{- ########################################################################################## -}
