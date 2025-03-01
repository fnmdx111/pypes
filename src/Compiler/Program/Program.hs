{-# LANGUAGE OverloadedStrings #-}

module Compiler.Program.Program
  (PypesExpr(..), PypesProgram(..), pypesProgramP) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Compiler.Token.Util (lexeme, program, Parser, charT)
import Control.Applicative (asum)
import Compiler.Program.VariablePatternMatch (VariablePatternMatch(..), vpmP)
import Compiler.Program.Literal (Literal(..), literalP)

-- data BinOp =
--   Add
--   | Minus
--   | Multiply

data PypesExpr =
  PipeExpr PypesExpr (Maybe VariablePatternMatch) PypesExpr
  | LitExpr Literal
  deriving (Show, Eq)

data PypesProgram = Expr PypesExpr
  deriving (Show, Eq)


pipeOperatorP :: Parser (Maybe VariablePatternMatch)
pipeOperatorP = charT '|' *> optional vpmP <* charT '>'

chainl1 :: (MonadParsec e s m) => m a -> m (a -> a -> a) -> m a
chainl1 pa op = do
    x <- pa
    rest x
  where
    rest x = (do
                 f <- op
                 y <- pa
                 rest (f x y))
             <|> pure x

pipeP :: Parser PypesExpr
pipeP = fmap LitExpr literalP `chainl1` (pipeOperatorP >>= \op -> pure $ \lhs rhs -> PipeExpr lhs op rhs)

exprP :: Parser PypesExpr
exprP = asum [ try pipeP
             , fmap LitExpr $ literalP
             ]

pypesProgramP :: Parser PypesProgram
pypesProgramP = program $ asum
  [ fmap Expr $ exprP
  ]
