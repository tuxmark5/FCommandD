module F.CommandD.Action.RubyMine
( rubyMineRun
) where

{- ########################################################################################## -}
import F.CommandD.Source.VirtualSource
{- ########################################################################################## -}

rubyMineRun             :: MacroM ()

rubyMineRun             = hold keyLeftShift $ downUp keyF10

{- ########################################################################################## -}
