module Compiler.Token.Identifier (identifier, Identifier(..)) where

import Text.Parsec
import Text.Parsec.String (Parser)

newtype Identifier = Identifier String deriving (Show, Eq)

identifier :: Parser Identifier
identifier = do
  leading <- choice [letter, char '-', char '_']
  rest <- many $ choice [alphaNum, char '-', char '_']
  modifier <- option "" $ string "?"
  return . Identifier $ [leading] ++ rest ++ modifier

