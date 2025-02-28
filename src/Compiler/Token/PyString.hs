{-# LANGUAGE OverloadedStrings #-}

module Compiler.Token.PyString (PyString(..), pyString) where

import Compiler.Token.Util (Parser, lexeme)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import Data.Char (toLower, toUpper)
import Data.Maybe (fromMaybe)

data PyString = PyUnicodeString Text

compositeStringPrefix :: Parser Text
compositeStringPrefix = do
  start <- oneOf ("fFrR" :: String)
  end <- oneOf [x | x <- "fFrR", toLower x /= start, toUpper x /= start]
  pure . pack $ start:[end]

simpleStringPrefix :: Parser Text
simpleStringPrefix = do
  prefix <- oneOf ("furFUR" :: String)
  pure . pack $ [prefix]

stringPrefix :: Parser Text
stringPrefix = try compositeStringPrefix <|> simpleStringPrefix

shortStringChar :: Char -> Parser Text
shortStringChar quote = do
  ch <- noneOf [ quote
               , '\n'
               , '\\'
               ]
  pure . pack $ [ch]

shortStringItem :: Char -> Parser Text
shortStringItem quote = try (shortStringChar quote) <|> stringEscapeSeq

stringEscapeSeq :: Parser Text
stringEscapeSeq = do
  esc <- char '\\'
  ch <- printChar
  pure . pack $ esc:[ch]

shortString :: Parser Text
shortString = do
  quote <- oneOf ("'\""::[Char])
  content <- many $ shortStringItem quote
  endQuote <- char quote
  pure $ (pack [quote]) <> (mconcat content) <> (pack [endQuote])

stringLiteral :: Parser Text
stringLiteral = do
  maybePrefix <- optional stringPrefix
  content <- shortString
  pure $ (fromMaybe "" maybePrefix) <> content

pyString :: Parser PyString
pyString = fmap PyUnicodeString (lexeme stringLiteral)
