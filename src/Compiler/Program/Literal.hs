module Compiler.Program.Literal
  (Literal(..), literalP) where

import Compiler.Token.Util (charT, Parser, lexeme)
import Compiler.Token.PyString (PyString(..), pyString)
import Compiler.Token.PyNumber (PyNumber(..), pyNumber)
import Compiler.Token.Identifier (Identifier(..), identifier)
import Text.Megaparsec (try, sepBy, (<?>))
import Control.Applicative (asum)
import Compiler.Token.Bool (PypesBool, boolT)
import Compiler.Token.None (PypesNone, noneT)

data Literal = LitString PyString
             | LitNumber PyNumber
             | LitList [Literal]
             | LitDict [(Literal, Literal)]
             | LitId Identifier
             | LitBool PypesBool
             | LitNone PypesNone
  deriving (Show, Eq)

litListP :: Parser Literal
litListP = do
  _ <- charT '[' <?> "start of list"
  xs <- literalP `sepBy` charT ','
  _ <- charT ']' <?> "end of list"
  pure $ LitList xs

litDictEntryP :: Parser (Literal, Literal)
litDictEntryP = do
  key <- literalP
  _ <- charT ':'
  value <- literalP
  pure (key, value)

litDictP :: Parser Literal
litDictP = do
  _ <- charT '{'
  xs <- litDictEntryP `sepBy` charT ','
  _ <- charT '}'
  pure $ LitDict xs

literalP :: Parser Literal
literalP = lexeme $ asum [ fmap LitString $ try pyString
                         , fmap LitNumber $ try pyNumber
                         , fmap LitId $ try identifier
                         , try litListP
                         , try litDictP
                         , fmap LitBool $ try boolT
                         , fmap LitNone $ try noneT
                         ]
