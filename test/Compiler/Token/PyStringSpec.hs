module Compiler.Token.PyStringSpec (spec) where

import Test.Hspec


spec :: Spec
spec = describe "PyString" $ do
  it "pass" $ do
    1 == 1

-- import Compiler.Token.PyString
-- import Test.Hspec
-- import Text.Parsec
-- import Control.Monad (forM_)

-- rawStringAcceptCases =
--   [ "r'abc\\n'"
--   , "r'abc\\''"]

-- test :: (String -> PyString) -> String -> SpecWith ()
-- test ctor str = do
--   it ("accepts " ++ str) $ do
--     case parse pyString "" str of
--       Left e -> fail $ "Expected parse to have succeeded but got " ++ show e
--       Right x -> x `shouldBe` (ctor str)

-- spec :: Spec
-- spec = do
--   describe "PyString" $ do
--     forM_ rawStringAcceptCases (test PyRawString)
