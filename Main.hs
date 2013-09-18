module Main where

import System.Environment (getArgs)

import Options (parseOptions)
import Sloch (sloch)

main :: IO ()
main = do
    (opts, [dir]) <- parseOptions =<< getArgs
    s <- sloch dir opts
    print s
