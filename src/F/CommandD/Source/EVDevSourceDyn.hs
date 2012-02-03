module F.CommandD.Source.EVDevSourceDyn
( EVDevMatcher
, EVDevSourceDyn(..)
, mkEVDevSourceDyn
) where

{- ########################################################################################## -}
import            Control.Exception (SomeException(..), handle)
import            Control.Monad (filterM, forM_)
import            Data.ByteString (readFile)
import            Data.ByteString.Lex.Integral
import            Data.Word (Word16)
import            F.CommandD.Daemon
import            F.CommandD.Source
import            F.CommandD.Source.EVDevSource
import            Prelude hiding (readFile)
import            System.Directory
import            System.Linux.Input (inputGrab, inputRead)
import            System.Posix.IO (OpenMode(..), defaultFileFlags, openFd)
import            System.Posix.Types (Fd)
{- ########################################################################################## -}

type EVDevMatcher = Word16 -> Word16 -> Bool

data EVDevSourceDyn = EVDevSourceDyn
  { esdMatcher :: EVDevMatcher
  }

instance SourceC EVDevSourceDyn where
  sourceRun s efun = do
    devs <- lift $ withDir "/sys/class/input" (match $ esdMatcher s)
    forM_ devs $ \dev -> do
      (Source src1) <- mkEVDevSource $ "/dev/input/" ++ dev
      forkCD $ sourceRun src1 efun
      
{- ########################################################################################## -}

match :: EVDevMatcher -> FilePath -> FilePath -> IO Bool
match m path file | head file == 'e' = 
  handle (\(SomeException _) -> return False) $ do
    let p = concat [path, '/':file, "/device/id/"]
    venStr  <- readFile $ p ++ "vendor"
    prodStr <- readFile $ p ++ "product"
    let (Just (venId,  _)) = readHexadecimal venStr
        (Just (prodId, _)) = readHexadecimal prodStr
    return $ m venId prodId
match _ _ _ = return False

mkEVDevSourceDyn :: EVDevMatcher -> CD (Source EVDevSourceDyn)
mkEVDevSourceDyn mt = return $ Source $ EVDevSourceDyn
  { esdMatcher  = mt
  }

withDir :: FilePath -> (FilePath -> FilePath -> IO Bool) -> IO [FilePath]
withDir dir fun = do
  entries <- getDirectoryContents dir
  filterM (\e -> case e of 
    '.':_       -> return False
    otherwise   -> fun dir e) entries
    
{- ########################################################################################## -}

