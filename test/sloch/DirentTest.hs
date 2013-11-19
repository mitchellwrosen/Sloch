module DirentTest (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Dirent

import Data.Maybe (isJust)

tests :: TestTree
tests = testGroup "Dirent"
    [ testCase "specs" spec
    ]

spec :: Spec
spec = do
    describe "makeDirent" $ do
        it "returns Nothing if the specified file doesn't exist" $ do
            dirent <- makeDirent False "non-existent-file"
            dirent `shouldBe` Nothing

        it "includes dotfiles when include_dotfiles is True" $ do
            dirent <- makeDirent True "test/sloch/DirentTest/.foo"
            isJust dirent `shouldBe` True

        it "ignores dotfiles when include_dotfiles is False" $ do
            dirent <- makeDirent False "test/sloch/DirentTest/.foo"
            dirent `shouldBe` Nothing

        it "ignores symlinks" $ do
            dirent <- makeDirent False "test/sloch/DirentTest/symlink"
            dirent `shouldBe` Nothing

        it "returns Nothing when the file cannot be opened" $ do
            dirent <- makeDirent False "test/sloch/DirentTest/no-read-permission"
            dirent `shouldBe` Nothing

        it "ignores . and .. entries of a directory" $ do
            let path = "test/sloch/DirentTest/empty-directory"
            dirent <- makeDirent False path
            dirent `shouldBe` Just (DirentDir path [])

        it "recurses into directories" $ do
            let path = "test/sloch/DirentTest/two-deep-dir"
            dirent <- makeDirent False path
            dirent `shouldBe` Just (
                DirentDir path [
                    DirentDir (path ++ "/foo") [
                        DirentFile (path ++ "/foo/bar")
                    ]
                ])
