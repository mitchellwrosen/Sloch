module Main where

import Test.Tasty

import qualified DirentTest

main = defaultMain $ testGroup "Tests"
    [ DirentTest.tests
    ]
