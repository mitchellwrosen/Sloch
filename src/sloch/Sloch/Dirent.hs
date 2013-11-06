module Sloch.Dirent
    ( SlochDirent(..)
    , slochDirent
    , slochDirents
    ) where

import Data.Maybe (catMaybes)

import Language
import Dirent
import LineCounter

data SlochDirent = SlochDirentFile FilePath Language Int
                 | SlochDirentDir FilePath [SlochDirent]
                 deriving Show


-- | Transform an ordinary Dirent into a SlochDirent, which is essentially the
-- same dirent, but annotated with line count information. Returns Nothing if
-- no lines could be counted, i.e. the file has an unknown file extension.
-- Directories work as expected, recursively.
slochDirent :: Dirent -> IO (Maybe SlochDirent)
slochDirent (DirentFile path) =
    case language path of
        Nothing   -> return Nothing
        Just lang -> countLines path lang >>= return . Just . SlochDirentFile path lang
slochDirent (DirentDir path children) =
    fmap catMaybes (mapM slochDirent children) >>= return . Just . SlochDirentDir path

slochDirents :: [Dirent] -> IO [SlochDirent]
slochDirents = fmap catMaybes . mapM slochDirent
