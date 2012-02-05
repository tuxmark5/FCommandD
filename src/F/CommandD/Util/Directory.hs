module F.CommandD.Util.Directory
( mapDir
) where
  
{- ########################################################################################## -}
import Data.Maybe (catMaybes)
import System.Directory
{- ########################################################################################## -}

mapDir :: FilePath -> (FilePath -> FilePath -> IO (Maybe a)) -> IO [a]
mapDir dir fun = do
  entries <- getDirectoryContents dir
  mapM (fun dir) entries >>= return . catMaybes 

{- ########################################################################################## -}
