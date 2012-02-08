{-# LANGUAGE OverloadedStrings #-}

module F.CommandD.Action.Guayadeque
( gdqNext
, gdqPlayPause
, gdqPrev
, gdqStop
) where
  
{- ########################################################################################## -}
import F.CommandD.Action.DBus
{- ########################################################################################## -}

gcall :: MemberName -> [Variant] -> MethodCall
gcall = newCall "org.mpris.guayadeque" "/Player" "org.freedesktop.MediaPlayer"

gdqNext                 :: MethodCall
gdqPlayPause            :: MethodCall
gdqPrev                 :: MethodCall
gdqStop                 :: MethodCall

gdqNext                 = gcall "Next" []
gdqPlayPause            = gcall "Pause" []
gdqPrev                 = gcall "Prev" []
gdqStop                 = gcall "Stop" []

{- ########################################################################################## -}
