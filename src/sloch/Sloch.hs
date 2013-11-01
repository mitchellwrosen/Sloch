{-# LANGUAGE LambdaCase, RankNTypes #-}

module Sloch
    ( LangToSloc
    , PathToLangToSloc
    , sloch
    , slochDirents
    , summarize
    , summarize'
    ) where

import Data.Map (Map)
import Data.Maybe (catMaybes)

import qualified Data.Map as M

import Data.Map.Extras (adjustWithDefault)
import Dirent (Dirent(..), makeDirent, direntsAtDepth)
import Language (Language, language)
import LineCounter (countLines)

data SlochDirent = SlochDirentFile FilePath Language Int
                 | SlochDirentDir FilePath [SlochDirent]
                 deriving Show

type PathToLangToSloc = Map FilePath LangToSloc
type LangToSloc       = Map Language [(FilePath, Int)]

sloch :: Int -> Bool -> FilePath -> IO PathToLangToSloc
sloch depth include_dotfiles path =
    makeDirent include_dotfiles path >>= \case
        Nothing -> return M.empty
        Just dirent -> do
            let dirents = direntsAtDepth depth dirent
            slochDirents dirents >>= return . summarize

slochDirents :: [Dirent] -> IO [SlochDirent]
slochDirents = fmap catMaybes . mapM slochDirent

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

-- | Summarize a list of SlochDirents, where each element is summarized and put
-- into a summary map. Each element is therefore the context with which the
-- children (if they exist) shall be inserted into the map with.
summarize :: [SlochDirent] -> PathToLangToSloc
summarize = foldr step M.empty
  where
    step :: SlochDirent -> PathToLangToSloc -> PathToLangToSloc
    step (SlochDirentFile path lang count) = M.insert path $ M.singleton lang [(path, count)]
    step (SlochDirentDir path children)    = M.insert path $ summarize' children

-- | Similar to summarize, but flattens a list of dirents into a single
-- LangToSloc map. No context (or "root" dirent) is associated with any of the
-- dirents in the list - they are all simply folded up uniformly.
summarize' :: [SlochDirent] -> LangToSloc
summarize' = foldr step M.empty
  where
    step :: SlochDirent -> LangToSloc -> LangToSloc
    step (SlochDirentFile path lang count) = adjustWithDefault (x:) lang [x] where x = (path,count)
    step (SlochDirentDir _ children)       = M.unionWith (++) $ summarize' children
