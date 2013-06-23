{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Language where

import Control.Lens ((&), (.~), (%~), makeLenses)

import qualified Data.ByteString.Char8 as B
import qualified Data.Map              as Map

-- A LineFilter is run on each line after it has been stripped of
-- whitespace. Each matching line is deleted.
--
type LineFilter = B.ByteString -> Bool

data Language =
   Language { _lName           :: B.ByteString                 -- Name of the language
            , _lLineComment    :: B.ByteString                 -- Line comment sequence
            , _lBlockComment   :: (B.ByteString, B.ByteString) -- (begin block comment, end block comment)
            , _lBoilerPlate    :: [LineFilter]                 -- Boiler plate lines that don't "count" as real code
            }

makeLenses ''Language

is :: Eq a => a -> a -> Bool
is = (==)

-- langInfoMap
--
-- Map from file extension to Language, representing all filetypes Sloch
-- knows about.
--
langInfoMap :: Map.Map String Language
langInfoMap = Map.fromList [ ("c",      cLanguage)
                           , ("cc",     cppLanguage)
                           , ("cpp",    cppLanguage)
                           , ("java",   javaLanguage)
                           , ("js",     javascriptLanguage)
                           , ("go",     goLanguage)
                           , ("groovy", groovyLanguage)
                           , ("h",      cHeaderLanguage)
                           , ("hs",     haskellLanguage)
                           , ("py",     pythonLanguage)
                           , ("rb",     rubyLanguage)
                           ]

-- Template for languages with similar syntax to C.
cLikeLanguage :: Language
cLikeLanguage = Language
   { _lName         = "C-like"
   , _lLineComment  = "//"
   , _lBlockComment = ("/*", "*/")
   , _lBoilerPlate  = [ is "{"
                      , is "}"
                      , is ";"
                      ]
   }


cLanguage :: Language
cLanguage = cLikeLanguage &
   lName .~ "C" &
   lBoilerPlate %~ (B.isPrefixOf "#include" :)

cHeaderLanguage :: Language
cHeaderLanguage = cLanguage & lName .~ "C/C++ Header"

cppLanguage :: Language
cppLanguage = cLanguage & lName .~ "C++"

javaLanguage :: Language
javaLanguage = cLikeLanguage &
   lName .~ "Java" &
   lBoilerPlate %~ (B.isPrefixOf "import "  :) .
                   (B.isPrefixOf "package " :)

javascriptLanguage :: Language
javascriptLanguage = cLikeLanguage & lName .~ "Javascript"

goLanguage :: Language
goLanguage = cLikeLanguage &
   lName .~ "Go" &
   lBoilerPlate %~ (B.isPrefixOf "import "  :) .
                   (B.isPrefixOf "package " :) .
                   (is ")" :)

groovyLanguage :: Language
groovyLanguage = javaLanguage & lName .~ "Groovy"

haskellLanguage :: Language
haskellLanguage = Language
   { _lName         = "Haskell"
   , _lLineComment  = "--"
   , _lBlockComment = ("{-", "-}")
   , _lBoilerPlate  = [ B.isPrefixOf "import "
                      , B.isPrefixOf "module "
                      , B.isInfixOf   "::" -- type annotations
                      , is "do"
                      , is "in"
                      , is "let"
                      , is "where"
                      , is "{"
                      , is "}"
                      , is "["
                      , is "]"
                      ]
   }

pythonLanguage :: Language
pythonLanguage = Language
   { _lName         = "Python"
   , _lLineComment  = "#"
   , _lBlockComment = ("'''", "'''")
   , _lBoilerPlate  = [ B.isPrefixOf "import "
                      , B.isPrefixOf "from "   -- from Foo import Bar
                      ]
   }

rubyLanguage :: Language
rubyLanguage = Language
   { _lName         = "Ruby"
   , _lLineComment  = "#"
   , _lBlockComment = ("=begin", "=end")
   , _lBoilerPlate  = [ B.isPrefixOf "load "
                      , B.isPrefixOf "require "
                      , B.isPrefixOf "require_relative "
                      , is "end "
                      , is "{"
                      , is "}"
                      ]
   }
