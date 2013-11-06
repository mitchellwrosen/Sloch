{-# LANGUAGE LambdaCase, RankNTypes #-}

module Sloch
    ( LangToSloc
    , PathToLangToSloc
    , sloch
    , summarize
    , summarize'
    ) where

import Data.Map (Map)

import qualified Data.Map as M

import Data.Map.Extras (adjustWithDefault)
import Dirent          (makeDirent, direntsAtDepth)
import Language        (Language)
import Sloch.Dirent    (SlochDirent(..), slochDirents)

type PathToLangToSloc = Map FilePath LangToSloc
type LangToSloc       = Map Language [(FilePath, Int)]

sloch :: Int -> Bool -> FilePath -> IO PathToLangToSloc
sloch depth include_dotfiles path =
    makeDirent include_dotfiles path >>= \case
        Nothing -> return M.empty
        Just dirent -> do
            let dirents = direntsAtDepth depth dirent
            slochDirents dirents >>= return . summarize

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
