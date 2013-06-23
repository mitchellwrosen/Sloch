{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Language where

import Control.Lens ((.~), makeLenses)

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
langInfoMap = Map.fromList [ ("c",  cLanguage      )
                           , ("go", goLanguage     )
                           , ("h",  cHeaderLanguage)
                           , ("hs", haskellLanguage)
                           , ("py", pythonLanguage )
                           ]

cLanguage :: Language
cLanguage = Language
   { _lName         = "C"
   , _lLineComment  = "//"
   , _lBlockComment = ("/*", "*/")
   , _lBoilerPlate  = [ B.isPrefixOf "#include"
                      , is "{"
                      , is "}"
                      , is ";"
                      ]
   }

cHeaderLanguage :: Language
cHeaderLanguage = lName .~ "C Header" $ cLanguage

goLanguage :: Language
goLanguage = Language
   { _lName         = "Go"
   , _lLineComment  = "//"
   , _lBlockComment = ("/*", "*/")
   , _lBoilerPlate  = [ B.isPrefixOf "import " 
                      , B.isPrefixOf "package "
                      , is "}"
                      ]
   }

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
