{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module F.CommandD.Filter.HubFilter
( HubFilter(..)
, hubNew
, hubSetSink
) where

{- ########################################################################################## -}
import Data.IORef
import Data.Maybe
import F.CommandD.ContST
import F.CommandD.Daemon
import F.CommandD.Filter
import F.CommandD.Sink
{- ########################################################################################## -}

data HubFilter = HubFilter (IORef (Maybe SinkA))

instance SinkC HubFilter where
  sinkWrite (HubFilter var) = do
    val <- lift $ readIORef var
    case val of
      (Just (SinkA sink)) -> sinkWrite sink
      Nothing             -> return ()
    
hubNew :: CD (Filter HubFilter)
hubNew = lift $ do
  ref <- newIORef Nothing
  return $ Sink $ HubFilter ref

hubSetSink :: SinkC s => Sink HubFilter -> Sink s -> CD ()
hubSetSink (Sink (HubFilter var)) (Sink s) = lift $ do
  writeIORef var (Just $ SinkA s)

{- ########################################################################################## -}
