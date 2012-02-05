module F.CommandD.Filter.DebugFilter
( DebugFilter
, mkDebugFilter
) where

{- ########################################################################################## -}
import F.CommandD.ContST
import F.CommandD.Daemon
import F.CommandD.Filter
import System.IO
{- ########################################################################################## -}

data DebugFilter = DebugFilter

instance SinkC DebugFilter where
  sinkWrite _ = get >>= \e -> lift $ do
    putStrLn $ (show $ eventType e) ++ " <> " ++ (show $ eventCode e) ++ " <> " ++ (show $ eventValue e)

mkDebugFilter :: CD (Filter DebugFilter)
mkDebugFilter = return $ Sink $ DebugFilter
  
{- ########################################################################################## -}
