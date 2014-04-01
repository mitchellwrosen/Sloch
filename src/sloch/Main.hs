{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Process (readProcess)

import qualified Data.Map as M

import Cli          (Cli(..), OptVerbose, parseCli)
import Dirent       (makeDirents)
import Sloch        (PathToLangToSloc, sloch, summarize')
import Sloch.Dirent (slochDirents)
import Sloch.Show   (showLangToSloc, showPathToLangToSloc)

main :: IO ()
main = parseCli >>= main'

main' :: Cli -> IO ()
main' Cli{..}
    | cliGit    = gitSloch cliVerbose
    | otherwise = sloch cliDepth cliIncludeDotfiles cliDir >>= putStrLn . showPathToLangToSloc cliVerbose

gitSloch :: OptVerbose -> IO ()
gitSloch verbose =
    readProcess "git" ["ls-files"] "" >>=
    makeDirents True . lines          >>=
    slochDirents                      >>=
    putStr . showLangToSloc verbose . summarize'

slochFiles :: [FilePath] -> Bool -> IO PathToLangToSloc
slochFiles paths include_dotfiles = fmap M.unions $
    mapM (sloch 0 include_dotfiles) paths
