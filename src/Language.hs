module Language
    ( Language(..)
    , isLineComment
    , isBeginBlockComment
    , isEndBlockComment
    , language
    ) where

import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf)
import System.FilePath.Posix (takeExtension)

data Language = Abap
              | ActionScript
              | Ada
              | C
              | Cpp
              | Java
              | JavaScript
              | Go
              | Groovy
              | Haskell
              | Perl
              | Php
              | Python
              | Ruby
              | Shell
              deriving (Eq, Ord, Show)

isLineComment :: Language -> String -> Bool
isLineComment Abap         = isPrefixOf "*"
isLineComment ActionScript = isPrefixOf "//"
isLineComment Ada          = isPrefixOf "--"
isLineComment C            = isPrefixOf "//"
isLineComment Cpp          = isPrefixOf "//"
isLineComment Java         = isPrefixOf "//"
isLineComment JavaScript   = isPrefixOf "//"
isLineComment Go           = isPrefixOf "//"
isLineComment Groovy       = isPrefixOf "//"
isLineComment Haskell      = isPrefixOf "--"
isLineComment Perl         = isPrefixOf "#"
isLineComment Php          = isPrefixOf "//"
isLineComment Python       = isPrefixOf "#"
isLineComment Ruby         = isPrefixOf "#"
isLineComment Shell        = isPrefixOf "#"

isBeginBlockComment :: Language -> String -> Bool
isBeginBlockComment Abap         = const False
isBeginBlockComment ActionScript = isPrefixOf "/*"
isBeginBlockComment Ada          = const False
isBeginBlockComment C            = isPrefixOf "/*"
isBeginBlockComment Cpp          = isPrefixOf "/*"
isBeginBlockComment Java         = isPrefixOf "/*"
isBeginBlockComment JavaScript   = isPrefixOf "/*"
isBeginBlockComment Go           = isPrefixOf "/*"
isBeginBlockComment Groovy       = isPrefixOf "/*"
isBeginBlockComment Haskell      = isPrefixOf "{-"
isBeginBlockComment Perl         = const False -- Or is it?
isBeginBlockComment Php          = isPrefixOf "/*"
isBeginBlockComment Python       = isPrefixOf "'''"
isBeginBlockComment Ruby         = isPrefixOf "=begin"
isBeginBlockComment Shell        = const False

isEndBlockComment :: Language -> String -> Bool
isEndBlockComment Abap         = const False
isEndBlockComment ActionScript = isInfixOf "*/"
isEndBlockComment Ada          = const False
isEndBlockComment C            = isInfixOf "*/"
isEndBlockComment Cpp          = isInfixOf "*/"
isEndBlockComment Java         = isInfixOf "*/"
isEndBlockComment JavaScript   = isInfixOf "*/"
isEndBlockComment Go           = isInfixOf "*/"
isEndBlockComment Groovy       = isInfixOf "*/"
isEndBlockComment Haskell      = isInfixOf "-}"
isEndBlockComment Perl         = const False
isEndBlockComment Php          = isInfixOf "*/"
isEndBlockComment Python       = isInfixOf "'''"
isEndBlockComment Ruby         = isInfixOf "=end"
isEndBlockComment Shell        = const False

language :: FilePath -> Maybe Language
language = language' . map toLower . takeExtension
  where
    language' :: String -> Maybe Language
    language' ".abap"   = Just Abap
    language' ".ada"    = Just Ada
    language' ".as"     = Just ActionScript
    language' ".c"      = Just C
    language' ".cc"     = Just Cpp
    language' ".cpp"    = Just Cpp
    language' ".java"   = Just Java
    language' ".js"     = Just JavaScript
    language' ".go"     = Just Go
    language' ".groovy" = Just Groovy
    language' ".hs"     = Just Haskell
    language' ".php"    = Just Php
    language' ".pl"     = Just Perl
    language' ".py"     = Just Python
    language' ".rb"     = Just Ruby
    language' ".sh"     = Just Shell
    language' _         = Nothing
