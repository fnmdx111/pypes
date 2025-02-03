module Compiler.Token.PyNumber (pyNumber, PyNumber(..)) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char (digitToInt)

data PyNumber = PyInt Integer | PyFloat Double deriving (Show, Eq)

baseTenInteger :: Parser String
baseTenInteger = do
  s <- many1 digit
  case s of
    "0" -> return "0"
    x:_ | x /= '0' -> return s
    _ -> unexpected "Unexpected leading zero"

digitInteger :: String -> [Char] -> Integer -> Parser Integer
digitInteger prefix allowedDigits base = do
  _ <- string prefix
  digits <- many1 (oneOf allowedDigits)
  return $ baseToInt digits
  where baseToInt = foldl (\acc x -> acc * base + toInteger (digitToInt x)) 0

decimalInteger :: Parser Integer
decimalInteger = do
  s <- baseTenInteger
  return $ read s

binaryInteger :: Parser Integer
binaryInteger = digitInteger "0b" "01" 2

octalInteger :: Parser Integer
octalInteger = digitInteger "0o" "01234567" 8

hexInteger :: Parser Integer
hexInteger = digitInteger "0x" "0123456789abcdef" 16

sign :: Num a => Parser (a -> a)
sign = (try (char '-' >> return negate)) <|> (optional (char '+') >> return id)

integer :: Parser PyNumber
integer = do
  signFunc <- sign
  num <- try hexInteger <|> try octalInteger <|> try binaryInteger <|> decimalInteger
  return $ PyInt (signFunc num)

eNotation :: Parser Integer
eNotation = do
  _ <- oneOf "eE"
  PyInt exponentInt <- integer
  return exponentInt

float0 :: Parser String
float0 = do
  whole <- baseTenInteger
  _ <- char '.'
  return $ whole ++ ".0"

float1 :: Parser String
float1 = do
  _ <- char '.'
  fractional <- many1 digit
  return $ "0." ++ fractional

float2 :: Parser String
float2 = do
  whole <- baseTenInteger
  _ <- char '.'
  fractional <- many1 digit
  return (whole ++ "." ++ fractional)

float :: Parser PyNumber
float = do
  signFunc <- sign
  n <- try float2 <|> try float0 <|> float1
  exponentInt <- option 0 eNotation
  return $ PyFloat (signFunc . read $ n ++ "e" ++ show exponentInt)

pyNumber :: Parser PyNumber
pyNumber = try float <|> integer

