{-# LANGUAGE OverloadedStrings #-}

module Compiler.Token.Util (Parser, symbol, lexeme, program) where

import Text.Megaparsec (Parsec, eof, between, optional)
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "--" "--")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

program :: Parser a -> Parser a
program = between (optional space1) (optional $ space1 >> eof)
