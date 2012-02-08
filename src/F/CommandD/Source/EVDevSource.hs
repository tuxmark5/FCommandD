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
  { evsHandle     :: Fd
  , evsSourceId   :: SourceId
  }

instance SourceC EVDevSource where
  sourceRead s = do
    e <- lift $ inputRead (evsHandle s) 
    return e { eventSource = evsSourceId s }

mkEVDevSource :: FilePath -> SourceId -> CD (Source EVDevSource)
mkEVDevSource dev sid = lift $ do
  fd <- openFd dev ReadOnly Nothing defaultFileFlags
  setFdOption fd CloseOnExec True
  inputGrab fd True
  return $ Source $ EVDevSource
    { evsHandle     = fd
    , evsSourceId   = sid
    }

{- ########################################################################################## -}
