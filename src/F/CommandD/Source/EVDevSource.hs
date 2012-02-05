module F.CommandD.Source.EVDevSource
( EVDevSource
, mkEVDevSource
) where

{- ########################################################################################## -}
import            F.CommandD.Daemon
import            F.CommandD.Source
import            System.Linux.Input (inputGrab, inputRead)
import            System.Posix.IO (FdOption(..), OpenMode(..), defaultFileFlags, openFd, setFdOption)
import            System.Posix.Types (Fd)
{- ########################################################################################## -}

data EVDevSource = EVDevSource
  { handle :: Fd
  }

instance SourceC EVDevSource where
  sourceRead s = lift $ inputRead (handle s) >>= return

mkEVDevSource :: FilePath -> CD (Source EVDevSource)
mkEVDevSource dev = lift $ do
  fd <- openFd dev ReadOnly Nothing defaultFileFlags
  setFdOption fd CloseOnExec True
  inputGrab fd True
  return $ Source $ EVDevSource
    { handle = fd
    }

{- ########################################################################################## -}
