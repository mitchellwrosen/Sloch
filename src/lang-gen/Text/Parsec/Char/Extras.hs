{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Char.Extras where

import Control.Applicative ((*>), (<*))
import Data.Char (isSpace)
import Text.Parsec (ParsecT, Stream, between, char, letter, many, sepBy1, satisfy, spaces)

csv :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
csv = (`sepBy1` comma)

comma :: Stream s m Char => ParsecT s u m Char
comma = char ','

notSpace :: Stream s m Char => ParsecT s u m Char
notSpace = satisfy (not . isSpace)

notSpaces :: Stream s m Char => ParsecT s u m String
notSpaces = many notSpace

word :: Stream s m Char => ParsecT s u m String
word = many letter

token :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
token = between spaces spaces
