module Compiler.Program.VariablePatternMatch
  (VariablePatternMatch(..), Vpm(..), vpmP) where

import Compiler.Token.Util (Parser, charT, lexeme)
import Compiler.Program.Literal (Literal(..), literalP)
import Control.Applicative (asum)
import Text.Megaparsec (try, notFollowedBy, (<?>))
import Compiler.Token.Identifier (Identifier(..), identifier)
import Text.Megaparsec (optional)

data Vpm =
  VpmLiteral Literal
  | VpmSnoc Literal Literal
  | VpmPair Literal Literal
  | VpmElse
  deriving (Show, Eq)

data VariablePatternMatch = NamedVpm Identifier Vpm
                          | UnnamedVpm Vpm
                          deriving (Show, Eq)

vpmLiteralP :: Parser Vpm
vpmLiteralP = fmap VpmLiteral literalP

vpmSnocP :: Parser Vpm
vpmSnocP = do
  hd <- literalP
  _ <- charT ':'
  tl <- literalP
  pure $ VpmSnoc hd tl

vpmPairP :: Parser Vpm
vpmPairP = do
  one <- literalP
  _ <- charT ','
  two <- literalP
  pure $ VpmPair one two

vpmElseP :: Parser Vpm
vpmElseP = charT '_' >> pure VpmElse

patternP :: Parser Vpm
patternP = asum [ try vpmSnocP
                , try vpmPairP
                , try vpmElseP
                , try vpmLiteralP
                ]

vpmP :: Parser VariablePatternMatch
vpmP = lexeme $ do
  maybeName <- optional . try $ identifier <* charT '@'
  let ctor = case maybeName of
        Nothing -> UnnamedVpm
        Just ident -> NamedVpm ident
  p <- patternP
  notFollowedBy (charT '@') <?> "Unexpected '@' after pattern"
  pure (ctor p)
