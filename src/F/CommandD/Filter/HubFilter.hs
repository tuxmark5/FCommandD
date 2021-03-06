{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module F.CommandD.Filter.HubFilter
( HubFilter(..)
, hubSetSink
, newHub
) where

{- ########################################################################################## -}
import Data.IORef
import Data.Maybe
import F.CommandD.Core
import F.CommandD.Filter
import F.CommandD.Sink
import F.CommandD.Util.ContST
{- ########################################################################################## -}

data HubFilter = HubFilter (IORef (Maybe SinkA))

instance SinkC HubFilter where
  sinkWrite (HubFilter var) = do
    val <- lift $ readIORef var
    case val of
      (Just (SinkA sink)) -> sinkWrite sink
      Nothing             -> return ()
      
{- ########################################################################################## -}
    
hubSetSink :: SinkC s => HubFilter -> s -> IO ()
hubSetSink (HubFilter var) s = do
  writeIORef var (Just $ SinkA s)
  
newHub :: CD HubFilter
newHub = lift $ do
  ref <- newIORef Nothing
  return $ HubFilter ref

{- ########################################################################################## -}
