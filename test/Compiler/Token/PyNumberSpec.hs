{-# LANGUAGE OverloadedStrings #-}

module Compiler.Token.PyNumberSpec (spec) where

import Compiler.Token.PyNumber
import Test.Hspec
import Text.Megaparsec
import Control.Monad (forM_)
import Data.Text (Text, unpack)

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

test :: (Text -> PyNumber) -> Text -> SpecWith ()
test ctor testCase = do
  (it . unpack) ("accepts " <> testCase) $ do
    case parse pyNumber "" testCase of
      Left e -> fail $ "Expected parse to have succeeded but got " <> show e
      Right x -> x `shouldBe` (ctor testCase)

spec :: Spec
spec = do
  describe "PyNumber" $ do
    forM_ integerTestCases (test PyInt)
    forM_ floatTestCases (test PyFloat)
    forM_ parseFailureCases $ \testCase -> do
      (it . unpack) ("rejects " <> testCase) $ do
        case parse pyNumber "" testCase of
          Left e -> (show e) `shouldSatisfy` (\x -> length x > 0)
          Right x -> fail $ "Unexpected successful parse " <> show x

