module System.FilePath.Extras (isDotfile) where

import System.FilePath.Posix (splitPath)
import Data.List (isPrefixOf)

-- Begins with ".", and isn't "." or ".."
isDotfile :: FilePath -> Bool
isDotfile path = case safeLast (splitPath path) of
    Nothing    -> False 
    Just path' -> isDotfile' path'

isDotfile' :: FilePath -> Bool
isDotfile' path = isPrefixOf "." path && path `notElem` [".", "..", "./", "../"]

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)
