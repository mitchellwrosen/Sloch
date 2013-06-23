module FilePathUtils where

import System.FilePath (takeExtension)

-- takeExtension_ target
--
-- Takes the extension from |target|, also dropping the '.'
takeExtension_ :: FilePath -> String 
takeExtension_ = drop 1 . takeExtension
