{-# LANGUAGE OverloadedStrings #-}

module F.CommandD.Action.XMonad
( xmonadCoreExit
, xmonadCoreRestart
, xmonadCoreSetWMName
, xmonadLayoutExpand
, xmonadLayoutNext
, xmonadLayoutShrink
, xmonadMasterFocus
, xmonadMasterMod
, xmonadMasterSwap
, xmonadNavGridSelect
, xmonadNavGo
, xmonadNavSwap
, xmonadScreenMoveWin
, xmonadScreenNext
, xmonadScreenSetCurr
, xmonadScreenSetWk
, xmonadScreenSwapNext
, xmonadTabMerge
, xmonadTabNext
, xmonadTabPrev
, xmonadTabUnmerge
, xmonadWinClose
, xmonadWinMoveTo
, xmonadWinSink
, xmonadWkMoveWindow
, xmonadWkMoveWindowG
, xmonadWkSetCurrent
, xmonadWkSetCurrentG
) where
  
{- ########################################################################################## -}
import Data.Int (Int32)
import F.CommandD.Action.DBus
{- ########################################################################################## -}

xcall :: InterfaceName -> MemberName -> [Variant] -> MethodCall
xcall = newCall "f.xmonad" "/xmonad"

xmonadCoreExit          :: MethodCall
xmonadCoreRestart       :: MethodCall
xmonadCoreSetWMName     :: String -> MethodCall
xmonadLayoutExpand      :: MethodCall
xmonadLayoutNext        :: MethodCall
xmonadLayoutShrink      :: MethodCall
xmonadMasterFocus       :: MethodCall
xmonadMasterMod         :: Int32 -> MethodCall
xmonadMasterSwap        :: MethodCall
xmonadNavGridSelect     :: MethodCall
xmonadNavGo             :: Int32 -> MethodCall
xmonadNavSwap           :: Int32 -> MethodCall
xmonadScreenMoveWin     :: Int32 -> MethodCall
xmonadScreenNext        :: MethodCall
xmonadScreenSetCurr     :: Int32 -> MethodCall
xmonadScreenSetWk       :: Int32 -> String -> MethodCall
xmonadScreenSwapNext    :: MethodCall
xmonadTabMerge          :: Int32 -> MethodCall
xmonadTabNext           :: MethodCall
xmonadTabPrev           :: MethodCall
xmonadTabUnmerge        :: MethodCall
xmonadWinClose          :: MethodCall
xmonadWinMoveTo         :: String -> MethodCall
xmonadWinSink           :: MethodCall
xmonadWkMoveWindow      :: String -> MethodCall
xmonadWkMoveWindowG     :: String -> MethodCall
xmonadWkSetCurrent      :: String -> MethodCall
xmonadWkSetCurrentG     :: String -> MethodCall

xmonadCoreExit          = xcall "f.xmonad.core"   "exit"        []
xmonadCoreRestart       = xcall "f.xmonad.core"   "restart"     []
xmonadCoreSetWMName n   = xcall "f.xmonad.core"   "setWMName"   [toVariant n]
xmonadLayoutExpand      = xcall "f.xmonad.layout" "expand"      []
xmonadLayoutNext        = xcall "f.xmonad.layout" "next"        []
xmonadLayoutShrink      = xcall "f.xmonad.layout" "shrink"      []
xmonadMasterFocus       = xcall "f.xmonad.master" "focus"       []
xmonadMasterMod d       = xcall "f.xmonad.master" "mod"         [toVariant d]
xmonadMasterSwap        = xcall "f.xmonad.master" "swap"        []
xmonadNavGridSelect     = xcall "f.xmonad.nav"    "gridSelect"  []
xmonadNavGo d           = xcall "f.xmonad.nav"    "go"          [toVariant d] -- NN
xmonadNavSwap d         = xcall "f.xmonad.nav"    "swap"        [toVariant d] -- NN
xmonadScreenMoveWin s   = xcall "f.xmonad.screen" "moveWin"     [toVariant s]
xmonadScreenNext        = xcall "f.xmonad.screen" "next"        []
xmonadScreenSetCurr s   = xcall "f.xmonad.screen" "setCurr"     [toVariant s]
xmonadScreenSetWk s w   = xcall "f.xmonad.screen" "setWk"       [toVariant s, toVariant w]
xmonadScreenSwapNext    = xcall "f.xmonad.screen" "swapNext"    []
xmonadTabMerge d        = xcall "f.xmonad.tab"    "merge"       [toVariant d] -- NN
xmonadTabNext           = xcall "f.xmonad.tab"    "next"        [] 
xmonadTabPrev           = xcall "f.xmonad.tab"    "prev"        []
xmonadTabUnmerge        = xcall "f.xmonad.tab"    "unmerge"     []
xmonadWinClose          = xcall "f.xmonad.win"    "close"       []
xmonadWinMoveTo w       = xcall "f.xmonad.win"    "moveToWk"    [toVariant w]
xmonadWinSink           = xcall "f.xmonad.win"    "sink"        []
xmonadWkMoveWindow w    = xcall "f.xmonad.wk"     "moveWin"     [toVariant w]
xmonadWkMoveWindowG w   = xcall "f.xmonad.wk"     "moveWinG"    [toVariant w]
xmonadWkSetCurrent w    = xcall "f.xmonad.wk"     "setCurr"     [toVariant w]
xmonadWkSetCurrentG w   = xcall "f.xmonad.wk"     "setCurrG"    [toVariant w]

{- ########################################################################################## -}
