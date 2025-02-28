{-# LANGUAGE OverloadedStrings #-}

module Compiler.Program.TestSpec (spec) where

import Compiler.Token.Identifier (Identifier(..), identifier)
import Compiler.Token.PyString (PyString(..), pyString)
import Compiler.Token.PyNumber (PyNumber(..), pyNumber)
import Compiler.Token.Util (Parser, program)
import Test.Hspec (Spec, describe)
import Compiler.Token.TestUtil (assertTokenParserAccepts, assertTokenParserRejects)
import Text.Megaparsec (optional)
import Data.Text (Text)
import Control.Monad (forM_)

data Program = Simple PyNumber PyNumber (Maybe PyString) Identifier
  deriving (Show, Eq)

testParser :: Parser Program
testParser = program $ do
  ident <- identifier
  num1 <- pyNumber
  maybeString <- optional pyString
  num2 <- pyNumber
  pure $ Simple num1 num2 maybeString ident


acceptedCases :: [(Text, Program)]
acceptedCases =
  [ ( "abc 123 1.0e10"
    , Simple (PyInt "123") (PyFloat "1.0e10") Nothing (Identifier "abc") )
  , ( "a-b_c? 123 'I\\\'m a comment' 456."
    , Simple (PyInt "123") (PyFloat "456.") (Just (PyUnicodeString "'I\\\'m a comment'")) (Identifier "a-b_c?") )
  ]

rejectedCases :: [Text]
rejectedCases =
  [ "123 1.0e10"
  , "abc"
  ]

spec :: Spec
spec = describe "test program" $ do
  forM_ acceptedCases $ \(testCase, expected) -> assertTokenParserAccepts testParser (const expected) testCase
  forM_ rejectedCases $ assertTokenParserRejects testParser
