{-# LANGUAGE LambdaCase, RankNTypes #-}

module Sloch
    ( Sloch
    , showSloch
    , showSlochHierarchy
    , SlochHierarchy
    , slochHierarchy
    , slochFiles
    ) where

import Control.Monad (forever, unless)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.State.Strict (StateT, modify)
import Data.List (sort, sortBy)
import Data.Monoid ((<>))
import Pipes (Consumer', Effect, Pipe, Producer', (>->), await, each, runEffect, yield)
import Pipes.Lift (execStateP)

import qualified Data.Map as M

import Cli (OptVerbose)
import Control.Monad.Extras (whenMaybe)
import Data.Map.Extras (adjustWithDefault)
import DirectoryTree (Dirent(..), makeDirent, direntsAtDepth)
import Language (Language, language)
import LineCounter (countLines)
import Sloch.Types (Sloch, SlochHierarchy)

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

showSlochHierarchy :: OptVerbose -> SlochHierarchy -> String
showSlochHierarchy verbose = unlines . concatMap display . sort . M.toList
  where
    display :: (FilePath, Sloch) -> [String]
    display (path, s) = path : map ("   " ++) (lines $ showSloch verbose s)

slochHierarchy :: FilePath -> Int -> Bool -> IO SlochHierarchy
slochHierarchy path depth include_dotfiles = do
    makeDirent path include_dotfiles >>= \case
        Nothing     -> return M.empty
        Just dirent -> do
            let trees = direntsAtDepth depth dirent
            execStateEffect M.empty $
                hoist lift (each trees) >-> slochHierarchy'

execStateEffect :: Monad m => s -> Effect (StateT s m) () -> m s
execStateEffect init_state effect = runEffect $ execStateP init_state $ effect

-- | "Outer" lines count, which builds up a mapping from dirent -> inner lines count. Only adds an entry if there were
-- any lines counted.
slochHierarchy' :: Consumer' Dirent (StateT SlochHierarchy IO) ()
slochHierarchy' = forever $ do
    await >>= \case
        DirentDir (path, children) -> foo path (hoist lift (each children) >-> direntToFilePath >-> sloch)
        DirentFile path            -> foo path (yield path >-> sloch)
  where
    foo :: FilePath -> Effect (StateT Sloch IO) () -> Consumer' Dirent (StateT SlochHierarchy IO) ()
    foo path effect = do
        lang_to_count_map <- liftIO $ execStateEffect M.empty $ effect
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
