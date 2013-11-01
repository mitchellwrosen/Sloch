module DirentTest (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Dirent

tests :: TestTree
tests = testGroup "Dirent"
    [ testCase "Spec" spec
    ]

spec :: Spec
spec = do
    describe "make dirent" $ do
        it "returns nothing if the specified file doesn't exist" $ do
            dirent <- makeDirent "asdf" False
            dirent `shouldBe` Nothing
