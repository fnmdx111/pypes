{-# LANGUAGE OverloadedStrings #-}

module Compiler.Token.BoolSpec (spec) where

import Test.Hspec
import Compiler.Token.Bool (boolT, PypesBool(..))
import Data.Text (Text)
import Compiler.Token.Util (Parser, program)
import Compiler.Token.TestUtil (assertTokenParserAccepts, assertTokenParserRejects)
import Control.Monad (forM_)

testParser :: Parser PypesBool
testParser = program boolT

acceptCases :: [(Text, PypesBool)]
acceptCases =
  [ ("T", PypesTrue)
  , ("True", PypesTrue)
  , ("F", PypesFalse)
  , ("False", PypesFalse)
  ]

rejectCases :: [Text]
rejectCases =
  [ "true"
  , "false"
  , "abc"
  ]

spec :: Spec
spec = do
  forM_ acceptCases $ \(input, expected) -> assertTokenParserAccepts testParser (const expected) input
  forM_ rejectCases $ assertTokenParserRejects testParser
