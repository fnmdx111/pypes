{-# LANGUAGE OverloadedStrings #-}

module Compiler.Token.IdentifierSpec (spec) where

import Compiler.Token.Identifier
import Test.Hspec
import Text.Megaparsec
import Control.Monad (forM_)
import Data.Text (Text, unpack)

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
  ]

spec :: Spec
spec = describe "Identifier" $ do
  forM_ acceptCases $ \testCase -> (it . unpack) ("accepts " <> testCase <> " as identifier") $ do
    case parse identifier "" testCase of
      Left e -> fail $ "Expected parse to have succeeded but got " ++ show e
      Right x -> x `shouldBe` (Identifier testCase)
  forM_ rejectCases $ \testCase -> (it . unpack) ("rejects " <> testCase <> " as identifier") $ do
    case parse identifier "" testCase of
      Left e -> (show e) `shouldSatisfy` (\x -> length x > 0)
      Right x -> fail $ "Unexpected successful parse " ++ show x

