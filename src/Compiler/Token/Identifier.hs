module Compiler.Token.Identifier (identifier, Identifier(..)) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Compiler.Token.Util
import Data.Text (Text, pack)

newtype Identifier = Identifier Text deriving (Show, Eq)

identifier :: Parser Identifier
identifier = lexeme $ do
  leading <- choice [letterChar, char '-', char '_']
  rest <- many $ choice [alphaNumChar, char '-', char '_']
  modifier <- optional (char '?') >>= \x -> case x of
    Just ch -> pure [ch]
    Nothing -> pure []
  pure . Identifier . pack $ [leading] ++ rest ++ modifier

