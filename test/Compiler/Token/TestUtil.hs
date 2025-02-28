{-# LANGUAGE OverloadedStrings #-}

module Compiler.Token.TestUtil
  ( assertTokenParserAccepts
  , assertTokenParserRejects ) where

import Test.Hspec (SpecWith, shouldBe, it, Expectation)
import Data.Text (Text, unpack)
import Compiler.Token.Util (Parser)
import Text.Megaparsec (parse)

success :: Expectation
success = pure ()

assertTokenParserAccepts :: (Show a, Eq a) => Parser a -> (Text -> a) -> Text -> SpecWith ()
assertTokenParserAccepts testParser ctor input = do
  (it . unpack) ("accepts " <> input) $ do
    let r = parse testParser "" input
    examineResult r
  let wsTestCase = "  " <> input <> "  "
  (it . unpack) ("accepts " <> wsTestCase) $ do
    let r = parse testParser "" wsTestCase
    examineResult r
  where
    examineResult (Left e) = fail $ "Expected successful parse but got " <> show e
    examineResult (Right x) = x `shouldBe` (ctor input)

assertTokenParserRejects :: (Show a) => Parser a -> Text -> SpecWith ()
assertTokenParserRejects testParser input = do
  (it . unpack) ("rejects " <> input) $ do
    let r = parse testParser "" input
    examineResult r
  let wsTestCase = "  " <> input <> "  "
  (it . unpack) ("rejects " <> wsTestCase) $ do
    let r = parse testParser "" wsTestCase
    examineResult r
  where
    examineResult (Right x) = fail $ "Expected unsuccessful parse but got " <> show x
    examineResult (Left _) = success
  
