module System.FilePath.Extras where

import Data.List (isPrefixOf)

isDotfile :: FilePath -> Bool
isDotfile = isPrefixOf "."
