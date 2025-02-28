{-# LANGUAGE OverloadedStrings #-}

module Compiler.Token.IdentifierSpec (spec) where

import Compiler.Token.Identifier
import Test.Hspec
import Control.Monad (forM_)
import Data.Text (Text)
import Compiler.Token.Util (Parser, program)
import Compiler.Token.TestUtil (assertTokenParserAccepts, assertTokenParserRejects)

acceptCases :: [Text]
acceptCases =
  [ "abc"
  , "-abc"
  , "-abc?"
  , "-abc-?"
  , "-abc_?"
  , "-abc10_?"
  , "abc-abc"
  , "aA"
  ]

rejectCases :: [Text]
rejectCases =
  [
    "0a"
  , "1-a"
  , ""
  , "Abc"
  , "None"
  , "Nil"
  , "Null"
  , "True"
  , "T"
  , "False"
  , "F"
  ]

testParser :: Parser Identifier
testParser = program identifier

spec :: Spec
spec = describe "Identifier" $ do
  forM_ acceptCases $ assertTokenParserAccepts testParser Identifier
  forM_ rejectCases $ assertTokenParserRejects testParser
