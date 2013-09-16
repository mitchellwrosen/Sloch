module Language
    ( Language(..)
    , isLineComment
    , isBeginBlockComment
    , isEndBlockComment
    , language
    ) where

import Data.List (isInfixOf, isPrefixOf)
import System.FilePath.Posix (takeExtension)

data Language = C
              | Cpp
              | Java
              | Javascript
              | Go
              | Groovy
              | Haskell
              | Python
              | Ruby
              | Shell

isLineComment :: Language -> String -> Bool
isLineComment C          = isPrefixOf "//"
isLineComment Cpp        = isPrefixOf "//"
isLineComment Java       = isPrefixOf "//"
isLineComment Javascript = isPrefixOf "//"
isLineComment Go         = isPrefixOf "//"
isLineComment Groovy     = isPrefixOf "//"
isLineComment Haskell    = isPrefixOf "--"
isLineComment Python     = isPrefixOf "#"
isLineComment Ruby       = isPrefixOf "#"
isLineComment Shell      = isPrefixOf "#"

isBeginBlockComment :: Language -> String -> Bool
isBeginBlockComment C          = isPrefixOf "/*"
isBeginBlockComment Cpp        = isPrefixOf "/*"
isBeginBlockComment Java       = isPrefixOf "/*"
isBeginBlockComment Javascript = isPrefixOf "/*"
isBeginBlockComment Go         = isPrefixOf "/*"
isBeginBlockComment Groovy     = isPrefixOf "/*"
isBeginBlockComment Haskell    = isPrefixOf "{-"
isBeginBlockComment Python     = isPrefixOf "'''"
isBeginBlockComment Ruby       = isPrefixOf "=begin"
isBeginBlockComment Shell      = const False

isEndBlockComment :: Language -> String -> Bool
isEndBlockComment C          = isInfixOf "*/"
isEndBlockComment Cpp        = isInfixOf "*/"
isEndBlockComment Java       = isInfixOf "*/"
isEndBlockComment Javascript = isInfixOf "*/"
isEndBlockComment Go         = isInfixOf "*/"
isEndBlockComment Groovy     = isInfixOf "*/"
isEndBlockComment Haskell    = isInfixOf "-}"
isEndBlockComment Python     = isInfixOf "'''"
isEndBlockComment Ruby       = isInfixOf "=end"
isEndBlockComment Shell      = const False

language :: FilePath -> Maybe Language
language = language' . takeExtension
  where
    language' :: String -> Maybe Language
    language' ".c"      = Just C
    language' ".cc"     = Just Cpp
    language' ".cpp"    = Just Cpp
    language' ".java"   = Just Java
    language' ".js"     = Just Javascript
    language' ".go"     = Just Go
    language' ".groovy" = Just Groovy
    language' ".hs"     = Just Haskell
    language' ".py"     = Just Python
    language' ".rb"     = Just Ruby
    language' ".sh"     = Just Shell
    language' _         = Nothing
