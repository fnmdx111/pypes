{-# LANGUAGE OverloadedStrings #-}

module Compiler.Token.NoneSpec (spec) where

import Test.Hspec
import Compiler.Token.None (noneT, PypesNone(..))
import Data.Text (Text)
import Compiler.Token.Util (Parser, program)
import Compiler.Token.TestUtil (assertTokenParserAccepts, assertTokenParserRejects)
import Control.Monad (forM_)

testParser :: Parser PypesNone
testParser = program noneT

acceptCases :: [(Text, PypesNone)]
acceptCases =
  [ ("None", PypesNone)
  , ("Nil", PypesNone)
  , ("Null", PypesNone)
  ]

rejectCases :: [Text]
rejectCases =
  [ "true"
  , "false"
  , "abc"
  , "null"
  ]

spec :: Spec
spec = do
  forM_ acceptCases $ \(input, expected) -> assertTokenParserAccepts testParser (const expected) input
  forM_ rejectCases $ assertTokenParserRejects testParser
