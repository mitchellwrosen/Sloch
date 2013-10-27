module Parser (parseLanguagesFile) where

import Control.Applicative ((<$>), (<$), (<*>), (*>), (<*), (<|>), liftA2)
import Control.Monad (void)
import Text.Parsec
    ( Parsec
    , ParseError
    , char
    , eof
    , many
    , newline
    , parse
    , sepBy1
    , space
    , spaces
    , string
    , try
    , upper
    )

import LanguageGen
import Text.Parsec.Char.Extras (csv, notSpaces, token, word)

type Parser = Parsec String ()

parseLanguagesFile :: FilePath -> IO (Either ParseError [LanguageGen])
parseLanguagesFile = fmap (parse languages "") . readFile

languages :: Parser [LanguageGen]
languages = many (language <* spaces) <* eof

language :: Parser LanguageGen
language = LanguageGen <$>
    (name    <* newline) <*>
    (exts    <* newline) <*>
    (comment <* newline) <*>
    (comment <* newline) <*>
    comment

name :: Parser String
name = liftA2 (:) upper word

exts :: Parser [String]
exts = threeSpaces *> word `sepBy1` (char ' ')

comment :: Parser [String]
comment = threeSpaces *> (noComment <|> yesComment)
  where
    noComment  = [] <$ try (string "none")
    yesComment = notSpaces `sepBy1` (char ' ')

threeSpaces :: Parser ()
threeSpaces = void $ space >> space >> space
