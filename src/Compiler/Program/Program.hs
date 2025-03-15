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
import qualified Text.Megaparsec.Debug as ParsecDebug
import Data.Text

dbg :: (Show a) => String -> Parser a -> Parser a
-- dbg = ParsecDebug.dbg
dbg _ = id

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
  | EqualTo
  deriving (Show, Eq)

data LambdaDefExpr = LambdaDefExpr VariablePatternMatch PypesExpr deriving (Show, Eq)
data FunInvocationExpr = FunInvocationExpr PypesExpr [PypesExpr] deriving (Show, Eq)
data PipeRhsExpr =
  Lambda [LambdaDefExpr]
  | FunRhs FunInvocationExpr
  deriving (Show, Eq)

data PipeLhsExpr =
  Lit PypesExpr
  | FunLhs FunInvocationExpr
  | Parenthesized PypesExpr
  deriving (Show, Eq)

data PipeExpr = PipeLhs PipeLhsExpr | PipeRhs PipeRhsExpr | PipeBin PipeExpr PipeExpr deriving (Show, Eq)


data PypesExpr =
  PypesPipeExpr PipeExpr
  | LitExpr Literal
  | FunExpr FunInvocationExpr
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
    , EqualTo <$ op '='
    ]
  where op = try . charT
        op' = try . chunkT


lambdaDefExprP :: Parser LambdaDefExpr
lambdaDefExprP = LambdaDefExpr <$> vpmP <* charT '$' <*> exprP

funInvocationArgP :: Parser PypesExpr
funInvocationArgP = dbg "fun invocation arg" $ (try lPartialBinaryFunExprP <|> try rPartialBinaryFunExprP <|> try (LitExpr <$> literalP) <|> try (paren exprP))
funInvocationExprP :: Parser FunInvocationExpr
funInvocationExprP = dbg "fun invocation" $ (FunInvocationExpr <$> functorP <*> many funInvocationArgP)

pipeRhsExprP :: Parser PipeRhsExpr
pipeRhsExprP = dbg "pipe rhs" $ ((try 
  (Lambda <$> (lambdaDefExprP `sepBy1` charT ';')) <|> (FunRhs <$> funInvocationExprP)) <?> "pipe rhs")

pipeLhsExprP :: Parser PipeLhsExpr
pipeLhsExprP = dbg "pipe lhs" $ (
  try (FunLhs <$> funInvocationExprP) <|> (try (Lit . LitExpr <$> literalP)
  <|> try (Parenthesized <$> paren exprP)) <?> "pipe lhs") <* lookAhead pipeOperatorP

pipeP :: Parser PipeExpr
pipeP = dbg "pipeP" $ (chain (PipeLhs <$> pipeLhsExprP) (PipeBin <$ pipeOperatorP) (PipeRhs <$> pipeRhsExprP))

lPartialBinaryFunExprP :: Parser PypesExpr
lPartialBinaryFunExprP = dbg "lpartial" $ (paren $ LeftPartialBinaryOpFunExpr <$> exprP <*> binOpP)

rPartialBinaryFunExprP :: Parser PypesExpr
rPartialBinaryFunExprP = dbg "rpartial" $ (paren $ RightPartialBinaryOpFunExpr <$> binOpP <*> exprP)

functorP :: Parser PypesExpr
functorP = dbg "functor" $ (try (LitExpr . LitId <$> identifier) <|> try (paren exprP) <?> "fun functor")

funExprP :: Parser PypesExpr
funExprP = dbg "funExprP" $ (try (FunExpr <$> funInvocationExprP) <?> "fun invocation")

pipeOperatorP :: Parser ()
pipeOperatorP = chunkT "|>" >> pure ()

chain :: (MonadParsec e s m) => m a -> m (a -> a -> a) -> m a -> m a
chain lhsP opP rhsP = do
    lhs <- lhsP
    rest lhs
  where
    rest x = (do
                 f <- opP
                 y <- rhsP
                 rest (f x y)) <|> pure x


pipeExprP :: Parser PypesExpr
pipeExprP = PypesPipeExpr <$> pipeP

paren :: Parser a -> Parser a
paren = between (charT '(') (charT ')')

exprP :: Parser PypesExpr
exprP = dbg "expr" $ lexeme $ asum
  [
    dbg "pipe" $ (try pipeExprP              <?> "pipe")
  , dbg "paren pipe" (try (paren pipeExprP)      <?> "paren pipe")
  , dbg "fun" $ (try funExprP           <?> "fun")
  , dbg "paren fun" $ (try (paren funExprP)   <?> "paren fun")
  , try lPartialBinaryFunExprP <?> "l-partial fun"
  , try rPartialBinaryFunExprP <?> "r-partial fun"
  ,  try (LitExpr <$> literalP)          <?> "literal"
  , try (LitExpr <$> paren literalP) <?> "paren literal"
  ]

pypesProgramP :: Parser PypesProgram
pypesProgramP = dbg "program" $ (program $ asum
  [ fmap Expr $ exprP
  ])
