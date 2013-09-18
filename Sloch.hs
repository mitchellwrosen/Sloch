{-# LANGUAGE LambdaCase, RankNTypes #-}

module Sloch (Sloch, sloch) where

import Control.Applicative ((<$>))
import Control.Monad (unless)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT, execStateT, modify, put)
import Data.Map (Map)

import qualified Data.Map      as M

import DirectoryTree (Dirent(..), DirectoryTree, makeTree, treesAtDepth)
import Language (Language, language)
import LineCounter (countLines)

-- A summary of the source-lines-of-code count, represented as two maps: the outer, a directory name to counts, and the
-- inner, a map from language type to number of lines.
type Sloch = Map FilePath (Map Language Int)

sloch :: FilePath -> Int -> IO Sloch
sloch path depth = do
    trees <- treesAtDepth depth <$> makeTree path
    execStateT (mapM_ sloch' trees) M.empty

-- | "Outer" lines count, which builds up a mapping from directory name -> inner lines count. Only adds an entry if
-- there were any lines counted.
sloch' :: DirectoryTree -> StateT Sloch IO ()
sloch' (path, children) = do
    lang_to_count_map <- lift $ execStateT (slochDirents children) M.empty
    unless (M.null lang_to_count_map) $
        modify $ M.insert path lang_to_count_map

-- | "Inner" lines count, which recursively builds up a language -> line count map from all entries in the specified
-- directory tree.
slochDirents :: [Dirent] -> StateT (Map Language Int) IO ()
slochDirents = mapM_ slochDirent

-- | "Inner" lines count, as above.
slochDirent :: Dirent -> StateT (Map Language Int) IO ()
slochDirent (DirentDir (_, children)) = slochDirents children
slochDirent (DirentFile file_path) =
    whenMaybe (language file_path) $ \lang -> do
        n <- lift $ countLines file_path lang
        modify $ M.insertWith (+) lang n

-- | Like when, but conditional on a Maybe being Just rather than a Bool being True.
whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe = flip $ maybe $ return ()
