{-# LANGUAGE LambdaCase #-}

module Main where

import Text.Hastache
import Text.Hastache.Context

import LanguageGen
import Parser

import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = parseLanguagesFile "languages" >>= \case
    Left err    -> error $ "parse error " ++ (show err)
    Right langs -> foo langs

foo langs = do
    res <- hastacheFile config "Language.mustache" (mkStrContext context)
    BS.putStrLn res
  where
    config = defaultConfig { muEscapeFunc = emptyEscape }

    context "langAdt"            = MuVariable $ makeAdt                langs
    context "lineComments"       = MuVariable $ makeLineComments       langs
    context "beginBlockComments" = MuVariable $ makeBeginBlockComments langs
    context "endBlockComments"   = MuVariable $ makeEndBlockComments   langs
    context "languageExts"       = MuVariable $ makeLanguages          langs
