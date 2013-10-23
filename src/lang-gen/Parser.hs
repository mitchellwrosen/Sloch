module Parser where

import Control.Applicative
import Text.Parsec hiding ((<|>), many)

{-import Language (Language(..))-}
import Language

type Parser = Parsec String ()

parseLanguagesFile :: FilePath -> IO (Either ParseError [Language])
parseLanguagesFile = fmap (parse languages "") . readFile

languages :: Parser [Language]
languages = many language <* eof

language :: Parser Language
language = spacesAfter $ Language <$> name <*> exts <*> comments <*> comments <*> comments

name :: Parser String
name = spacesAfter $ liftA2 (:) upper (many letter)

exts :: Parser [String]
exts = spacesAfter $ sepBy1 (many letter) (char ',')

comments :: Parser [String]
comments = spacesAfter $
    [] <$ try (string "none") <|>
    sepBy1 (many (noneOf " \t\r\n")) (char ',')

spacesAfter :: Parser a -> Parser a
spacesAfter p = p <* spaces
