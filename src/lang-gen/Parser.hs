module Parser (parseLanguagesFile) where

import Control.Applicative ((<$>), (<$), (<*>), (<*), (<|>), liftA2)
import Text.Parsec (Parsec, ParseError, eof, many, parse, string, try, upper)

import Language
import Text.Parsec.Char.Extras (csv, notSpaces, token, word)

type Parser = Parsec String ()

parseLanguagesFile :: FilePath -> IO (Either ParseError [Language])
parseLanguagesFile = fmap (parse languages "") . readFile

languages :: Parser [Language]
languages = many language <* eof

language :: Parser Language
language = token $ Language <$> name <*> exts <*> comment <*> comment <*> comment

name :: Parser String
name = token $ liftA2 (:) upper word

exts :: Parser [String]
exts = token $ csv word

comment :: Parser [String]
comment = token $ noComment <|> yesComment
  where
    noComment  = [] <$ try (string "none")
    yesComment = csv notSpaces
