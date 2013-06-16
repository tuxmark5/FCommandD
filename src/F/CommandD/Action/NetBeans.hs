module F.CommandD.Action.NetBeans
( nbQuickOpenFile
) where

{- ########################################################################################## -}
import F.CommandD.Source.VirtualSource
{- ########################################################################################## -}

nbQuickOpenFile       :: MacroM ()

nbQuickOpenFile       = hold keyRightAlt $ hold keyRightShift $ downUp keyO

{- ########################################################################################## -}
