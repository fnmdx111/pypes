{-# LANGUAGE OverloadedStrings #-}

module Compiler.Token.PyNumberSpec (spec) where

import Compiler.Token.PyNumber
import Compiler.Token.Util
import Test.Hspec
import Control.Monad (forM_)
import Data.Text (Text)
import Compiler.Token.TestUtil (assertTokenParserAccepts, assertTokenParserRejects)

integerTestCases :: [Text]
integerTestCases =
  [ "123"
  , "0"
  , "0b01010"
  , "0o01234"
  , "0x0ffff"
  ]

floatTestCases :: [Text]
floatTestCases =
  [ "3.14"
  , "2.718"
  , "0.123"
  , "1.234e10"
  , "1.234e-10"
  , "1."
  , ".1"
  , "0."
  , ".0"
  , "0.0"
  , "01.0"
  ]

parseFailureCases :: [Text]
parseFailureCases =
  [ "."
  , ""
  , "abcdef"
  , "012345"
  ]

testParser :: Parser PyNumber
testParser = program pyNumber

spec :: Spec
spec = do
  describe "PyNumber" $ do
    forM_ integerTestCases $ assertTokenParserAccepts testParser PyInt
    forM_ floatTestCases $ assertTokenParserAccepts testParser PyFloat
    forM_ parseFailureCases $ assertTokenParserRejects testParser
