module LanguageGen
    ( LanguageGen(..)
    , makeAdt
    , makeLineComments
    , makeBeginBlockComments
    , makeEndBlockComments
    , makeLanguages
    ) where

import Data.List (intersperse)

data LanguageGen = LanguageGen
    { langName               :: !String
    , langExts               :: [String]
    , langLineComments       :: [String]
    , langBeginBlockComments :: [String]
    , langEndBlockComments   :: [String]
    } deriving Show

-- | Make the ADT for the LanguageGens.
makeAdt :: [LanguageGen] -> String
makeAdt = concat . intersperse "\n   | " . map langName

-- | Make the comment definitions.
makeLineComments, makeBeginBlockComments, makeEndBlockComments :: [LanguageGen] -> String
makeLineComments       = withCommentFunc isLineComment
makeBeginBlockComments = withCommentFunc isBeginBlockComment
makeEndBlockComments   = withCommentFunc isEndBlockComment

withCommentFunc :: (LanguageGen -> String) -> [LanguageGen] -> String
withCommentFunc f = unlines . map f

isLineComment, isBeginBlockComment, isEndBlockComment :: LanguageGen -> String
isLineComment       lang = commentFunc "isLineComment "       (langName lang) (langLineComments lang)       isPrefix
isBeginBlockComment lang = commentFunc "isBeginBlockComment " (langName lang) (langBeginBlockComments lang) isPrefix
isEndBlockComment   lang = commentFunc "isEndBlockComment "   (langName lang) (langEndBlockComments lang)   isInfix

commentFunc :: String -> String -> [String] -> ([String] -> String) -> String
commentFunc func_name lang_name lang_comments func = func_name ++ lang_name ++ " = " ++ func lang_comments

isPrefix, isInfix :: [String] -> String
isPrefix = commentFuncDefinition "isPrefixOf"
isInfix  = commentFuncDefinition "isInfixOf"

commentFuncDefinition :: String -> [String] -> String
commentFuncDefinition _ []  = "const False"
commentFuncDefinition f [x] = f ++ " \"" ++ x ++ "\""
commentFuncDefinition f xs  = "or . ([" ++ funcs xs ++ "] <*>) . pure"
  where
    funcs :: [String] -> String
    funcs = concat . intersperse ", " . map (\s -> f ++ " \"" ++ s ++ "\"")

-- | Make the "language" function.
makeLanguages :: [LanguageGen] -> String
makeLanguages = unlines . concat . map makeLanguage

makeLanguage :: LanguageGen -> [String]
makeLanguage lang = map (makeLanguage' (langName lang)) (langExts lang)
  where
    makeLanguage' :: String -> String -> String
    makeLanguage' name ext = "    language' \"." ++ ext ++ "\" = Just " ++ name
