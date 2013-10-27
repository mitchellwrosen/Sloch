module Language where

import Data.List (intersperse)

data Language = Language
    { langName               :: !String
    , langExts               :: [String]
    , langLineComments       :: [String]
    , langBeginBlockComments :: [String]
    , langEndBlockComments   :: [String]
    } deriving Show

langAdt :: [Language] -> String
langAdt = concat . intersperse " | " . map langName

isLineComment, isBeginBlockComment, isEndBlockComment :: Language -> String
isLineComment       lang = commentFunc "isLineComment"       (langName lang) (langLineComments lang)       isPrefix
isBeginBlockComment lang = commentFunc "isBeginBlockComment" (langName lang) (langBeginBlockComments lang) isPrefix
isEndBlockComment   lang = commentFunc "isEndBlockComment"   (langName lang) (langEndBlockComments lang)   isInfix

commentFunc :: String -> String -> [String] -> ([String] -> String) -> String
commentFunc func_name lang_name lang_comments func = func_name ++ lang_name ++ " = " ++ func lang_comments

isPrefix, isInfix :: [String] -> String
isPrefix = commentFuncDefinition "isPrefixOf"
isInfix  = commentFuncDefinition "isInfixOf"

commentFuncDefinition :: String -> [String] -> String
commentFuncDefinition _ []  = "const False"
commentFuncDefinition f [x] = f ++ " \"" ++ x ++ "\"\n"
commentFuncDefinition f xs  = "or . [" ++ funcs xs ++ "] <*>) . pure"
  where
    funcs :: [String] -> String
    funcs = concat . intersperse ", " . map (\s -> f ++ " \"" ++ s ++ "\"")
