{-# LANGUAGE OverloadedStrings #-}

module Compiler.Program.Program
  (PypesExpr(..), PypesProgram(..), pypesProgramP) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Compiler.Token.Util (lexeme, program, Parser, charT)
import Control.Applicative (asum)
import Compiler.Program.VariablePatternMatch (VariablePatternMatch(..), vpmP)
import Compiler.Program.Literal (Literal(..), literalP)

data PypesExpr =
  PipeExpr PypesExpr (Maybe VariablePatternMatch) PypesExpr
  | LitExpr Literal
  deriving (Show, Eq)

data PypesProgram = Expr PypesExpr
  deriving (Show, Eq)

pipeP :: Parser PypesExpr
pipeP = do
  lhs <- literalP
  _ <- charT '|'
  vpm <- optional vpmP
  _ <- charT '>'
  rhs <- exprP
  pure $ PipeExpr (LitExpr lhs) vpm rhs

exprP :: Parser PypesExpr
exprP = asum [ try pipeP
             , fmap LitExpr $ literalP
             ]

pypesProgramP :: Parser PypesProgram
pypesProgramP = program $ asum
  [ fmap Expr $ exprP
  ]
