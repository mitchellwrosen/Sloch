module Main where

import Pipes (each)
import System.Environment (getArgs)
import System.Process

import Options (Options(..), parseOptions)
import Sloch (slochFiles, slochHierarchy)

main :: IO ()
main = do
    (opts, nonopts) <- parseOptions =<< getArgs

    let depth            = optDepth opts
        include_dotfiles = optIncludeDotfiles opts
        git              = optGit opts

    if git
        then gitSloch
        else do
            let [dir] = nonopts
            s <- slochHierarchy dir depth include_dotfiles
            print s

gitSloch :: IO ()
gitSloch = do
    files <- readProcess "git" ["ls-files"] ""
    s <- slochFiles $ each (lines files)
    print s
