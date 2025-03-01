{-# LANGUAGE OverloadedStrings #-}

module Compiler.Program.Program
  (PypesExpr(..), PypesProgram(..), BinOp(..), pypesProgramP) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Compiler.Token.Util (lexeme, program, Parser, charT, chunkT)
import Control.Applicative (asum)
import Compiler.Program.VariablePatternMatch (VariablePatternMatch(..), vpmP)
import Compiler.Program.Literal (Literal(..), literalP)
import Compiler.Token.Identifier (Identifier(..), identifier)
import Text.Megaparsec.Debug (dbg, MonadParsecDbg)

data BinOp =
  Add
  | Minus
  | Multiply
  | Divide
  | TrueDivide
  | Power
  | NoneDefault  -- y ?? z -> z if y == None else y
  | GreaterThan
  | GreaterThanOrEqualTo
  | LessThan
  | LessThanOrEqualTo
  | NotEqualTo
  deriving (Show, Eq)

data PypesExpr =
  PipeExpr PypesExpr (Maybe VariablePatternMatch) PypesExpr
  | LitExpr Literal
  | FunExpr PypesExpr [PypesExpr]
  | LeftPartialBinaryOpFunExpr PypesExpr BinOp
  | RightPartialBinaryOpFunExpr BinOp PypesExpr
  deriving (Show, Eq)

data PypesProgram = Expr PypesExpr
  deriving (Show, Eq)

binOpP :: Parser BinOp
binOpP = lexeme . asum $
    [ Add <$ op '+'
    , Minus <$ op '-'
    , Multiply <$ op '*'
    , Divide <$ op' "//"
    , TrueDivide <$ op '/'
    , Power <$ op' "**"
    , NoneDefault <$ op' "??"
    , LessThan <$ op '<'
    , LessThanOrEqualTo <$ op' "<="
    , GreaterThan <$ op '>'
    , GreaterThanOrEqualTo <$ op' ">="
    , NotEqualTo <$ op' "!="
    ]
  where op = try . charT
        op' = try . chunkT

lPartialBinaryFunExprP :: Parser PypesExpr
lPartialBinaryFunExprP = paren $ LeftPartialBinaryOpFunExpr <$> exprP <*> binOpP

rPartialBinaryFunExprP :: Parser PypesExpr
rPartialBinaryFunExprP = paren $ RightPartialBinaryOpFunExpr <$> binOpP <*> exprP

functorP :: Parser PypesExpr
functorP = try (identifier >>= pure . LitExpr . LitId) <|> paren exprP

funExprP_noParen :: Parser PypesExpr
funExprP_noParen = FunExpr <$> functorP <*> many exprP <* (charT '$')

funExprP_paren :: Parser PypesExpr
funExprP_paren = paren $ FunExpr <$> functorP <*> many exprP <* (optional $ charT '$')

funExprP :: Parser PypesExpr
funExprP = try funExprP_paren <|> funExprP_noParen

pipeOperatorP :: Parser (Maybe VariablePatternMatch)
pipeOperatorP = (charT '|' <?> "start of pipe") *> optional vpmP <* (charT '>' <?> "end of pipe")

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

lassocBinP :: Parser (PypesExpr -> PypesExpr -> PypesExpr) -> Parser PypesExpr -> Parser PypesExpr
lassocBinP opP termP = (try (paren termP) <|> termP) `chainl1` opP

pipeP :: Parser PypesExpr
pipeP = lassocBinP opP termP
  where opP = do
          vpm <- pipeOperatorP
          pure $ \lhs rhs -> PipeExpr lhs vpm rhs
        termP = try (fmap LitExpr literalP) <|> funExprP

paren :: Parser a -> Parser a
paren = between (charT '(') (charT ')')

exprP :: Parser PypesExpr
exprP = lexeme $ foldl (<|>) empty
  [
    try lPartialBinaryFunExprP <?> "l-partial fun"
  , try rPartialBinaryFunExprP <?> "r-partial fun"
  , try (paren funExprP)   <?> "paren fun"
  , try (paren pipeP)      <?> "paren pipe"
  , LitExpr <$> try (paren literalP) <?> "paren literal"
  , try funExprP           <?> "fun"
  , try pipeP              <?> "pipe"
  , LitExpr <$> literalP          <?> "literal"
  ]

pypesProgramP :: Parser PypesProgram
pypesProgramP = program $ asum
  [ fmap Expr $ exprP
  ]
