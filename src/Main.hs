module Main where

import Pipes (each)
import System.Process (readProcess)

import Cli (Cli(..), OptVerbose, parseCli)
import Sloch (showSloch, showSlochHierarchy, slochFiles, slochHierarchy)

main :: IO ()
main = parseCli >>= main'

main' :: Cli -> IO ()
main' cli = do
    let depth            = cliDepth cli
        include_dotfiles = cliIncludeDotfiles cli
        git              = cliGit cli
        verbose          = cliVerbose cli
        dir              = cliDir cli

    if git
        then gitSloch verbose
        else do
            s <- slochHierarchy dir depth include_dotfiles
            putStr $ showSlochHierarchy verbose s

gitSloch :: OptVerbose -> IO ()
gitSloch verbose = do
    files <- readProcess "git" ["ls-files"] ""
    s <- slochFiles $ each (lines files)
    putStr $ showSloch verbose s
