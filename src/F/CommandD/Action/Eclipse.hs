module F.CommandD.Action.Eclipse
( eclipseFindReferences
, eclipseGoBack
, eclipseGoForward
, eclipseOpenDeclaration
) where

{- ########################################################################################## -}
import F.CommandD.Source.VirtualSource
{- ########################################################################################## -}

eclipseFindReferences   :: MacroM ()
eclipseGoBack           :: MacroM ()
eclipseGoForward        :: MacroM ()
eclipseOpenDeclaration  :: MacroM ()

eclipseFindReferences   = hold keyRightCtrl $ hold keyRightShift $ downUp keyG
eclipseGoBack           = hold keyRightAlt $ downUp keyLeft
eclipseGoForward        = hold keyRightAlt $ downUp keyRight
eclipseOpenDeclaration  = downUp keyF3

{- ########################################################################################## -}
