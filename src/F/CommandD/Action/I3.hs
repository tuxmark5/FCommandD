{-# LANGUAGE OverloadedStrings #-}

module F.CommandD.Action.I3 
( i3Cmd
, i3Mode
, i3MoveWk
, i3SetWk
) where

{- ########################################################################################## -}
import            Data.Binary
import            Data.Binary.Put
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as B
import qualified  Data.ByteString.Lazy.Char8 as BL
import qualified  Data.ByteString.Unsafe     as BSU
import            Foreign.Ptr (castPtr)
import            Network.Socket hiding (send, sendTo, recv, recvFrom)
import            Network.Socket.ByteString
import            System.IO
import            System.Posix.IO 
{- ########################################################################################## -}

i3Send :: String -> ByteString -> IO ()
i3Send addr buf = do
  soc <- socket AF_UNIX Stream 0
  connect soc (SockAddrUnix addr)
  send soc buf
  res <- recv soc 4096
  B.putStrLn res
  sClose soc

i3Msg :: String -> Put 
i3Msg msg = do
  putByteString "i3-ipc" 
  putWord32host $ fromIntegral $ (length msg) + 1
  putWord32host $ 0
  putByteString $ B.pack msg
  put '\0'

i3Cmd :: String -> IO ()
i3Cmd cmd = do
  let buf = (B.concat $ BL.toChunks $ runPut $ i3Msg cmd)
  i3Send "/home/angel/.i3/ipc.sock" buf
  B.writeFile "/tmp/xxx" buf
  putStrLn cmd

{- ########################################################################################## -}

i3Mode    :: String -> IO ()
i3MoveWk  :: String -> IO ()
i3SetWk   :: String -> IO ()

i3Mode    m = i3Cmd $ "mode \"" ++ m ++ "\""
i3MoveWk  w = i3Cmd $ "move container to workspace " ++ w
i3SetWk   w = i3Cmd $ "workspace " ++ w

{- ########################################################################################## -}
