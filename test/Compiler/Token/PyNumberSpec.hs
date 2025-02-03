module Compiler.Token.PyNumberSpec (spec) where

import Compiler.Token.PyNumber
import Test.Hspec
import Text.Parsec
import Control.Monad (forM_)

integerTestCases :: [(String, Integer)]
integerTestCases =
  [ ("123", 123)
  , ("0", 0)
  , ("+123", 123)
  , ("-456", -456)
  , ("0b01010", 10)
  , ("-0b01010", -10)
  , ("+0b01010", 10)
  , ("0o01234", 668)
  , ("-0o01234", -668)
  , ("+0o01234", 668)
  , ("0x0ffff", 65535)
  , ("-0x0ffff", -65535)
  , ("+0x0ffff", 65535)
  ]

floatTestCases :: [(String, Double)]
floatTestCases =
  [ ("3.14", 3.14)
  , ("-2.718", -2.718)
  , ("2.718", 2.718)
  , ("+2.718", 2.718)
  , ("0.123", 0.123)
  , ("1.234e10", 1.234e10)
  , ("-1.234e10", -1.234e10)
  , ("-1.234e-10", -1.234e-10)
  , ("1.234e-10", 1.234e-10)
  , ("+1.234e-10", 1.234e-10)
  , ("+1.234e+10", 1.234e+10)
  , ("1.", 1.0)
  , ("-1.", -1.0)
  , ("+1.", 1.0)
  , (".1", 0.1)
  , ("-.1", -0.1)
  , ("+.1", 0.1)
  , ("0.", 0.0)
  , (".0", 0.0)
  , ("0.0", 0.0)
  ]

parseFailureCases :: [String]
parseFailureCases =
  [ "."
  , ""
  , "abcdef"
  , "012345"
  , "01.0"
  ]

test :: (a -> PyNumber) -> (String, a) -> SpecWith ()
test ctor (actual, expected) = do
  it ("parses " ++ actual) $ do
    case parse pyNumber "" actual of
      Left e -> fail $ "Expected parse to have succeeded but got " ++ show e
      Right x -> x `shouldBe` (ctor expected)

spec :: Spec
spec = do
  describe "PyNumber" $ do
    forM_ integerTestCases (test PyInt)
    forM_ floatTestCases (test PyFloat)
    forM_ parseFailureCases $ \testCase -> do
      it ("does not accept " ++ testCase) $ do
        case parse pyNumber "" testCase of
          Left e -> (show e) `shouldSatisfy` (\x -> length x > 0)
          Right x -> fail $ "Unexpected successful parse " ++ show x

