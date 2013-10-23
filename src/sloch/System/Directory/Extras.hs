module System.Directory.Extras where

import System.FilePath ((</>))
import qualified System.Directory as D

-- | Get the fully qualified directory contents of the specified directory, excluding the "." and ".." entries.
getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path = fmap (map (path </>) . filter (`notElem` [".",".."])) $ D.getDirectoryContents path
