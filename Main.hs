module Main where

import Pipes (each)
import System.Process (readProcess)
import System.Environment (getArgs)

import Cli (Cli(..), parseCli)
import Sloch (showSloch, showSlochHierarchy, slochFiles, slochHierarchy)

main :: IO ()
main = parseCli >>= main'

main' :: Cli -> IO ()
main' cli =
    if cliGit cli
        then gitSloch
        else do
            s <- slochHierarchy (cliDir cli) (cliDepth cli) (cliIncludeDotfiles cli)
            putStr $ showSlochHierarchy s

gitSloch :: IO ()
gitSloch = do
    files <- readProcess "git" ["ls-files"] ""
    s <- slochFiles $ each (lines files)
    putStr $ showSloch s
