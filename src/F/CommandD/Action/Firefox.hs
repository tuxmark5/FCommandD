module F.CommandD.Action.Firefox
( firefoxGoBack
, firefoxGoForward
, firefoxOpenInNewTab
, firefoxSearchForSel
, firefoxTabClose
) where

{- ########################################################################################## -}
import F.CommandD.Source.VirtualSource
{- ########################################################################################## -}

firefoxGoBack           :: MacroM ()
firefoxGoForward        :: MacroM ()
firefoxOpenInNewTab     :: MacroM ()
firefoxSearchForSel     :: MacroM ()
firefoxTabClose         :: MacroM ()

firefoxGoBack           = hold keyRightAlt $ downUp keyLeft
firefoxGoForward        = hold keyRightAlt $ downUp keyRight
firefoxOpenInNewTab     = downUp btnRight >> delay 80 >> downUp keyT
firefoxSearchForSel     = downUp btnRight >> delay 80 >> downUp keyS
firefoxTabClose         = hold keyLeftCtrl $ downUp keyW

{- ########################################################################################## -}
