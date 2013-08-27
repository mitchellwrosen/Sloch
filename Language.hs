{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Language where

import Control.Lens ((&), (.~), (%~), makeLenses)

import qualified Data.Map  as Map
import qualified Data.Text as T

-- A LineFilter is run on each line after it has been stripped of
-- whitespace. Each matching line is deleted.
--
type LineFilter = T.Text -> Bool

data Language =
   Language { _lName           :: T.Text                 -- Name of the language
            , _lLineComment    :: T.Text                 -- Line comment delimiter
            , _lBlockComment   :: Maybe (T.Text, T.Text) -- Block comment delimiters, if they exist
            , _lBoilerPlate    :: [LineFilter]           -- Boiler plate lines that don't "count" as real code
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
                           , ("sh",     shellLanguage)
                           ]

-- Template for languages with similar syntax to C.
cLikeLanguage :: Language
cLikeLanguage = Language
   { _lName         = "C-like"
   , _lLineComment  = "//"
   , _lBlockComment = Just ("/*", "*/")
   , _lBoilerPlate  = [ is "{"
                      , is "}"
                      , is ";"
                      ]
   }


cLanguage :: Language
cLanguage = cLikeLanguage &
   lName .~ "C" &
   lBoilerPlate %~ (T.isPrefixOf "#include" :) .
                   (is "#ENDIF" . T.toUpper :)

cHeaderLanguage :: Language
cHeaderLanguage = cLanguage & lName .~ "C/C++ Header"

cppLanguage :: Language
cppLanguage = cLanguage & lName .~ "C++"

javaLanguage :: Language
javaLanguage = cLikeLanguage &
   lName .~ "Java" &
   lBoilerPlate %~ (T.isPrefixOf "import "  :) .
                   (T.isPrefixOf "package " :)

javascriptLanguage :: Language
javascriptLanguage = cLikeLanguage & lName .~ "Javascript"

goLanguage :: Language
goLanguage = cLikeLanguage &
   lName .~ "Go" &
   lBoilerPlate %~ (T.isPrefixOf "import "  :) .
                   (T.isPrefixOf "package " :) .
                   (is ")" :)

groovyLanguage :: Language
groovyLanguage = javaLanguage & lName .~ "Groovy"

haskellLanguage :: Language
haskellLanguage = Language
   { _lName         = "Haskell"
   , _lLineComment  = "--"
   , _lBlockComment = Just ("{-", "-}")
   , _lBoilerPlate  = [ T.isPrefixOf "import "
                      , T.isPrefixOf "module "
                      , T.isInfixOf   "::" -- type annotations
                      , is "#ENDIF" . T.toUpper
                      , is "do"
                      , is "in"
                      , is "let"
                      , is "where"
                      , is "{"
                      , is "}"
                      , is "["
                      , is "]"
                      , is "("
                      , is ")"
                      ]
   }

pythonLanguage :: Language
pythonLanguage = Language
   { _lName         = "Python"
   , _lLineComment  = "#"
   , _lBlockComment = Just ("'''", "'''")
   , _lBoilerPlate  = [ T.isPrefixOf "import "
                      , T.isPrefixOf "from "   -- from Foo import Bar
                      ]
   }

rubyLanguage :: Language
rubyLanguage = Language
   { _lName         = "Ruby"
   , _lLineComment  = "#"
   , _lBlockComment = Just ("=begin", "=end")
   , _lBoilerPlate  = [ T.isPrefixOf "load "
                      , T.isPrefixOf "require "
                      , T.isPrefixOf "require_relative "
                      , is "end "
                      , is "{"
                      , is "}"
                      ]
   }

shellLanguage :: Language
shellLanguage = Language
   { _lName         = "Shell script"
   , _lLineComment  = "#"
   , _lBlockComment = Nothing
   , _lBoilerPlate  = [ is "do"
                      , is "done"
                      , is "esac"
                      , is "fi"
                      ]
   }
