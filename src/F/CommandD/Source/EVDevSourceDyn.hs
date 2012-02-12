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
import            F.CommandD.Core
import            F.CommandD.Source
import            F.CommandD.Source.EVDevSource
import            F.CommandD.Util.Directory (mapDir)
import            Prelude hiding (readFile)
import            System.Directory
import            System.Linux.Input (inputGrab, inputRead)
import            System.Posix.IO (OpenMode(..), defaultFileFlags, openFd)
import            System.Posix.Types (Fd)
{- ########################################################################################## -}

type EVDevMatcher = Word16 -> Word16 -> IO SourceId

data EVDevSourceDyn = EVDevSourceDyn
  { esdMatcher :: EVDevMatcher
  }

instance SourceC EVDevSourceDyn where
  sourceRun s efun = do
    devs <- lift $ mapDir "/sys/class/input" (match $ esdMatcher s)
    forM_ devs $ \(dev, sid) -> do
      (Source src1) <- mkEVDevSource ("/dev/input/" ++ dev) sid
      forkCD $ sourceRun src1 efun
      
{- ########################################################################################## -}

match :: EVDevMatcher -> FilePath -> FilePath -> IO (Maybe (FilePath, SourceId))
match m path file | head file == 'e' = 
  handle (\(SomeException _) -> return Nothing) $ do
    let p = concat [path, '/':file, "/device/id/"]
    venStr  <- readFile $ p ++ "vendor"
    prodStr <- readFile $ p ++ "product"
    let (Just (venId,  _))  = readHexadecimal venStr
        (Just (prodId, _))  = readHexadecimal prodStr
    sid     <- m venId prodId
    return $ if sid >= 0
      then Just (file, sid)
      else Nothing
match _ _ _ = return Nothing

mkEVDevSourceDyn :: EVDevMatcher -> CD (Source EVDevSourceDyn)
mkEVDevSourceDyn mt = return $ Source $ EVDevSourceDyn
  { esdMatcher  = mt
  }
    
{- ########################################################################################## -}

