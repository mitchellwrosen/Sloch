{-# LANGUAGE LambdaCase, RankNTypes #-}

module Sloch
    ( Sloch
    , SlochHierarchy
    , slochHierarchy
    , slochFiles
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forever, unless)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.State.Strict (StateT, execStateT, modify, put)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Map (Map)
import Pipes (Consumer', Effect, Pipe, Producer', (>->), await, each, runEffect, yield)
import Pipes.Lift (execStateP)

import qualified Data.Map      as M
import qualified Pipes.Prelude as P

import DirectoryTree (Dirent(..), DirectoryTree, makeTree, treesAtDepth)
import Language (Language, language)
import LineCounter (countLines)
import Options (Options(..))

-- A simple summary of the source-lines-of-code count, by language.
type Sloch = Map Language Int

-- A summary of the source-lines-of-code count, represented as two maps: the outer, a directory name to counts, and the
-- inner, a map from language type to number of lines.
type SlochHierarchy = Map FilePath Sloch

slochHierarchy :: FilePath -> Int -> Bool -> IO SlochHierarchy
slochHierarchy path depth include_dotfiles = do
    trees <- treesAtDepth depth <$> makeTree path include_dotfiles
    execStateEffect M.empty $
        hoist lift (each trees) >-> slochHierarchy'

execStateEffect :: Monad m => s -> Effect (StateT s m) () -> m s
execStateEffect init_state effect = runEffect $ execStateP init_state $ effect

-- | "Outer" lines count, which builds up a mapping from directory name -> inner lines count. Only adds an entry if
-- there were any lines counted.
slochHierarchy' :: Consumer' DirectoryTree (StateT SlochHierarchy IO) ()
slochHierarchy' = do
    (path, children) <- await
    lang_to_count_map <-
        liftIO $
            execStateEffect M.empty $
                hoist lift (each children) >-> direntToFilePath >-> sloch

    lift $ unless (M.null lang_to_count_map) $
        modify $ M.insert path lang_to_count_map

direntToFilePath :: Monad m => Pipe Dirent FilePath m ()
direntToFilePath = forever $
    await >>=
        \case
            (DirentDir (_, children)) -> each children >-> direntToFilePath
            (DirentFile path)         -> yield path

-- Source-lines-of-code from a Producer input.
slochFiles :: Producer' FilePath IO () -> IO Sloch
slochFiles files = execStateEffect M.empty $ hoist lift files >-> sloch

-- Generic source-lines-of-code counter that consumes FilePaths in the Sloch state monad.
sloch :: Consumer' FilePath (StateT Sloch IO) ()
sloch = forever $ do
    file_path <- await
    whenMaybe (language file_path) $ \lang -> do
        n <- liftIO $ countLines file_path lang
        lift $ modify $ M.insertWith (+) lang n

-- | Like when, but conditional on a Maybe being Just rather than a Bool being True.
whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe = flip $ maybe $ return ()
