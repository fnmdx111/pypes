{-# LANGUAGE OverloadedStrings #-}

module Compiler.Program.LiteralSpec (spec) where

import Compiler.Token.Util (Parser, program)
import Test.Hspec (Spec, describe)
import Compiler.Token.TestUtil (assertTokenParserAccepts, assertTokenParserRejects)
import Compiler.Program.Literal (Literal(..), literalP)
import Compiler.Token.PyString (PyString(..))
import Compiler.Token.PyNumber (PyNumber(..))
import Compiler.Token.Bool (PypesBool(..))
import Compiler.Token.None (PypesNone(..))
import Compiler.Token.Identifier (Identifier(..))
import Data.Text (Text)
import Control.Monad (forM_)

testParser :: Parser Literal
testParser = program literalP

acceptCases :: [(Text, Literal)]
acceptCases =
  [ ( "[[1, None, T], abc, 4.e1, ['a']]"
    , LitList [ LitList [ LitNumber (PyInt "1")
                                     , LitNone PypesNone
                                     , LitBool PypesTrue
                                     ]
                           , LitId (Identifier "abc")
                           , LitNumber (PyFloat "4.e1")
                           , LitList [ LitString (PyUnicodeString "'a'")
                                     ]
                           ]
    )
  , ( "{a: b, \"c\": [d]}"
    , LitDict [ ( LitId (Identifier "a")
                , LitId (Identifier "b")
                )
              , ( LitString (PyUnicodeString "\"c\"")
                , LitList [ LitId (Identifier "d")
                          ]
                )
              ]
    )
  , ( "{  }"
    , LitDict []
    )
  , ( "[  ]"
    , LitList []
    )
  ]

rejectCases :: [Text]
rejectCases = ["{]", "[[[]]", "{{}}", "{a}", "{a:}"]

spec :: Spec
spec = do
  describe "Literal parser" $ do
    forM_ acceptCases $ \(input, expected) -> assertTokenParserAccepts testParser (const expected) input
    forM_ rejectCases (assertTokenParserRejects testParser)
