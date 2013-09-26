{-# LANGUAGE LambdaCase, RankNTypes #-}

module Sloch
    ( Sloch
    , showSloch
    , showSlochHierarchy
    , SlochHierarchy
    , slochHierarchy
    , slochFiles
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forever, unless)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.State.Strict (StateT, modify)
import Data.List (sort, sortBy)
import Data.Map (Map)
import Data.Monoid ((<>))
import Pipes (Consumer', Effect, Pipe, Producer', (>->), await, each, runEffect, yield)
import Pipes.Lift (execStateP)

import qualified Data.Map as M

import Cli (OptVerbose)
import DirectoryTree (Dirent(..), DirectoryTree, makeTree, treesAtDepth)
import Language (Language, language)
import LineCounter (countLines)

-- A simple summary of the source-lines-of-code count, by language.
type Sloch = Map Language [(FilePath, Int)]

showSloch :: OptVerbose -> Sloch -> String
showSloch verbose = unlines . map display . sortBy compareSloc . M.toList
  where
    compareSloc :: (Language, [(FilePath, Int)]) -> (Language, [(FilePath, Int)]) -> Ordering
    compareSloc (lang1, fs1) (lang2, fs2) = (sumSnds fs2) `compare` (sumSnds fs1) <> lang1 `compare` lang2

    display :: (Language, [(FilePath, Int)]) -> String
    display (lang, fs) = unlines $
        show lang : lineCounts
      where
        lineCounts :: [String]
        lineCounts =
            if verbose
                then map (\(fp,n) -> "      " ++ show n ++ " " ++ fp) (sortBy (\a b -> snd b `compare` snd a) fs)
                else ["      " ++ show (sumSnds fs)]

sumSnds :: Num b => [(a,b)] -> b
sumSnds = foldr ((+) . snd) 0

-- A summary of the source-lines-of-code count, represented as two maps: the outer, a directory name to counts, and the
-- inner, a map from language type to number of lines.
type SlochHierarchy = Map FilePath Sloch

showSlochHierarchy :: OptVerbose -> SlochHierarchy -> String
showSlochHierarchy verbose = unlines . concatMap display . sort . M.toList
  where
    display :: (FilePath, Sloch) -> [String]
    display (path, s) = path : map ("   " ++) (lines $ showSloch verbose s)

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
slochHierarchy' = forever $ do
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
        lift $ modify $ adjustWithDefault ((file_path, n):) lang [(file_path, n)]

-- | Like when, but conditional on a Maybe being Just rather than a Bool being True.
whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe = flip $ maybe $ return ()

adjustWithDefault :: Ord k => (a -> a) -> k -> a -> Map k a -> Map k a
adjustWithDefault f k a m = if M.member k m then M.adjust f k m else M.insert k a m
