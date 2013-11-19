{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import           System.Environment    ( getArgs )
import           Text.Hastache         ( MuType(..)
                                       , MuConfig(..)
                                       , defaultConfig
                                       , emptyEscape
                                       , hastacheFile
                                       )
import           Text.Hastache.Context ( mkStrContext )

import           LanguageGen           ( LanguageGen(..)
                                       , makeAdt
                                       , makeBeginBlockComments
                                       , makeEndBlockComments
                                       , makeLanguages
                                       , makeLineComments
                                       )
import           Parser                ( parseLanguagesFile )

main :: IO ()
main = do
    [dir] <- getArgs

    parseLanguagesFile (dir ++ "languages") >>= \case
        Left err    -> error $ "parse error " ++ (show err)
        Right langs -> main' dir langs

main' :: FilePath -> [LanguageGen] -> IO ()
main' dir langs = do
    res <- hastacheFile config (dir ++ "Language.mustache") (mkStrContext context)
    BS.putStrLn res
  where
    config = defaultConfig { muEscapeFunc = emptyEscape }

    context "langAdt"            = MuVariable $ makeAdt                langs
    context "lineComments"       = MuVariable $ makeLineComments       langs
    context "beginBlockComments" = MuVariable $ makeBeginBlockComments langs
    context "endBlockComments"   = MuVariable $ makeEndBlockComments   langs
    context "languageExts"       = MuVariable $ makeLanguages          langs
