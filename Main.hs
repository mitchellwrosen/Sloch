module Main where

import Pipes (each)
import System.Environment (getArgs)

import Cli (Cli(..), parseCli)
import Sloch (slochFiles, slochHierarchy)

main :: IO ()
main = parseCli >>= main'

main' :: Cli -> IO ()
main' cli =
    if cliGit cli
        then gitSloch
        else do
            s <- slochHierarchy (cliDir cli) (cliDepth cli) (cliIncludeDotfiles cli)
            print s

gitSloch :: IO ()
gitSloch = do
    files <- readProcess "git" ["ls-files"] ""
    s <- slochFiles $ each (lines files)
    print s
