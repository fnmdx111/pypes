{-# LANGUAGE OverloadedStrings #-}

module Compiler.Token.PyStringSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import Data.Text (Text)
import Compiler.Token.PyString (pyString, PyString(..))
import Compiler.Token.Util
import Compiler.Token.TestUtil (assertTokenParserAccepts, assertTokenParserRejects)

stringAcceptCases :: [Text]
stringAcceptCases =
  [ "r'abc\\n'"
  , "'abc\\n'"
  , "\"abc\\\"\"" ]

stringRejectCases :: [Text]
stringRejectCases =
  [ "'abc\""
  ]

testParser :: Parser PyString
testParser = program pyString

spec :: Spec
spec = describe "PyString" $ do
  forM_ stringAcceptCases $ assertTokenParserAccepts testParser PyUnicodeString
  forM_ stringRejectCases $ assertTokenParserRejects testParser
