{-# LANGUAGE OverloadedStrings #-}

module LangInfo where

import ByteStringUtils

import qualified Data.ByteString.Lazy.Char8 as B

-- A LineFilter is run on each line after it has been stripped of
-- whitespace. Each matching line is deleted.
--
type LineFilter = B.ByteString -> Bool

data LangInfo =
   LangInfo { lineComment  :: B.ByteString                 -- line comment sequence
            , blockComment :: (B.ByteString, B.ByteString) -- (begin block comment, end block comment)
            , boilerPlate  :: [LineFilter]                 -- boiler plate lines that don't "count" as real code
            }

is :: Eq a => a -> a -> Bool
is = (==)

cLangInfo = LangInfo cLineComment cBlockComment cBoilerPlate
cLineComment  = "//"
cBlockComment = ("/*", "*/")
cBoilerPlate  = [ B.isPrefixOf "#include"
                , is           "}"
                , is           "{"
                , is           ";"
                ]

---------------------------------------------------------------------------

haskellLangInfo = LangInfo haskellLineComment haskellBlockComment haskellBoilerPlate
haskellLineComment = "--"
haskellBlockComment = ("{-", "-}")
haskellBoilerPlate = [ B.isPrefixOf "module " -- module declaration
                     , B.isPrefixOf "import " -- import statement
                     , bIsInfixOf   "::"      -- type annotations
                     , is           "let"
                     , is           "where"
                     ]

