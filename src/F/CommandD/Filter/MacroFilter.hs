module F.CommandD.Filter.MacroFilter
( MacroFilter(..)
) where

{- ########################################################################################## -}
import F.CommandD.Filter
{- ########################################################################################## -}

data Mode = Mode
data Node = Node

data MacroFilter = MacroFilter
  { mfModes   :: [Mode]
  , mfNodes   :: [Node]
  }

instance FilterC MacroFilter where
  sinkWrite _ _ = return True

-- a) disable
-- b) single key
-- c) macro: launch program, key sequence, send dbus, change mode
  
-- MODE TREE


  
-- filter can be timer based, so it doesn't need initial event
-- Event + Chain == Event + [Sink]
-- Filters cannot introduce new events - that's what VirtualSource is for
-- but filters can either: 1) filter event completely 2) withold the event for some time and
-- then re-emit it 3) pass-through the event
 
-- Chain = Source [Filter] Sink
-- Chain = Source [Filter]

-- some monad might be nice for this: it should have writeEvent 
-- continuation?
-- arrow?

-- source thread handles the dispatch -> each source might need some MVar?

-- we need to know the source of event: (Event, Source)

{- ########################################################################################## -}
