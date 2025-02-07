{-# LANGUAGE OverloadedStrings #-}

module Compiler.Token.PyNumber (pyNumber, PyNumber(..)) where

import Compiler.Token.Util (Parser, lexeme)
import Data.Text (Text, pack)
import Text.Megaparsec
import Text.Megaparsec.Char


data PyNumber = PyInt Text | PyFloat Text deriving (Show, Eq)

nonZeroDigit :: Parser Char
nonZeroDigit = oneOf ['1'..'9'] <?> "non-zero digit"

optionalUnderscore :: Parser [Char]
optionalUnderscore = optional (char '_') >>= \x -> case x of
  Just underscore -> pure [underscore]
  Nothing -> pure []

pyDigit :: Parser Char -> Parser String
pyDigit digitParser = do
  lead <- optionalUnderscore
  rest <- digitParser
  return $ lead ++ [rest]

decInteger1 :: Parser PyNumber
decInteger1 = do
  lead <- nonZeroDigit
  rest <- many $ pyDigit digitChar
  return . PyInt . pack $ lead:(mconcat rest)

decInteger2 :: Parser PyNumber
decInteger2 = do
  lead <- some (char '0')
  rest <- many $ pyDigit (char '0')
  return . PyInt . pack $ lead ++ mconcat rest

decInteger :: Parser PyNumber
decInteger = try decInteger1 <|> decInteger2

modParser :: Parser Char -> Parser [Char]
modParser modChar = do
  lead <- char '0'
  next <- modChar
  return $ lead:[next]

binInteger :: Parser PyNumber
binInteger = do
  lead <- modParser $ oneOf ['b', 'B']
  rest <- some $ pyDigit binDigitChar
  return . PyInt . pack $ lead ++ mconcat rest

octInteger :: Parser PyNumber
octInteger = do
  lead <- modParser $ oneOf ['o', 'O']
  rest <- some $ pyDigit octDigitChar
  return . PyInt . pack $ lead ++ mconcat rest

hexInteger :: Parser PyNumber
hexInteger = do
  lead <- modParser $ oneOf ['x', 'X']
  rest <- some $ pyDigit hexDigitChar
  return . PyInt . pack $ lead ++ mconcat rest

integer :: Parser PyNumber
integer = try hexInteger <|> try octInteger <|> try binInteger <|> decInteger


digitPart :: Parser String
digitPart = do
  lead <- digitChar
  rest <- many $ pyDigit digitChar
  return $ lead:mconcat rest

fractionPart :: Parser String
fractionPart = do
  dot <- char '.'
  rest <- digitPart
  return $ dot:rest

exponentPart :: Parser [Char]
exponentPart = do
  e <- oneOf ['e', 'E']
  sign <- optional (oneOf ['+', '-']) >>= \ch -> case ch of
    Just c -> pure [c]
    Nothing -> pure []
  rest <- digitPart
  return $ e:sign ++ rest

pointFloat1 :: Parser String
pointFloat1 = do
  p1 <- option "" digitPart
  p2 <- fractionPart
  return $ p1 ++ p2

pointFloat2 :: Parser String
pointFloat2 = do
  p1 <- digitPart
  p2 <- char '.'
  return $ p1 ++ [p2]

pointFloat :: Parser String
pointFloat = try pointFloat1 <|> pointFloat2

exponentFloat :: Parser String
exponentFloat = do
  p1 <- choice [pointFloat, digitPart]
  p2 <- exponentPart
  return $ p1 ++ p2

float :: Parser PyNumber
float = (try exponentFloat <|> pointFloat) >>= return . PyFloat . pack

pyNumber :: Parser PyNumber
pyNumber = lexeme $ try float <|> integer

