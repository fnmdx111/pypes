{-# LANGUAGE OverloadedStrings #-}

module Compiler.Program.VariablePatternMatchSpec (spec) where

import Compiler.Token.Util (Parser, program)
import Test.Hspec (Spec, describe)
import Compiler.Token.TestUtil (assertTokenParserAccepts, assertTokenParserRejects)
import Compiler.Program.Literal (Literal(..))
import Compiler.Token.PyString (PyString(..))
import Compiler.Token.PyNumber (PyNumber(..))
import Compiler.Token.Bool (PypesBool(..))
import Compiler.Token.Identifier (Identifier(..))
import Data.Text (Text)
import Control.Monad (forM_)
import Compiler.Program.VariablePatternMatch (VariablePatternMatch(..), Vpm(..), vpmP)

testParser :: Parser VariablePatternMatch
testParser = program vpmP

acceptCases :: [(Text, VariablePatternMatch)]
acceptCases =
  [ ( "a"
    , UnnamedVpm $ VpmLiteral (LitId (Identifier "a"))
    )
  , ( "x:xs"
    , UnnamedVpm $ VpmSnoc (LitId (Identifier "x")) (LitId (Identifier "xs"))
    )
  , ( "fst, snd"
    , UnnamedVpm $ VpmPair (LitId (Identifier "fst")) (LitId (Identifier "snd"))
    )
  , ( "x:[y]"
    , UnnamedVpm $ VpmSnoc (LitId (Identifier "x")) (LitList [LitId (Identifier "y")])
    )
  , ( "\"abc\", {'d': 'ef'}"
    , UnnamedVpm $ VpmPair (LitString (PyUnicodeString "\"abc\"")) (LitDict [ (LitString (PyUnicodeString "'d'")
                                                                              , LitString (PyUnicodeString "'ef'")
                                                                              )
                                                                            ])
    )
  , ( "p@[x, 1]"
    , NamedVpm (Identifier "p") $ VpmLiteral (LitList [ LitId (Identifier "x")
                                       , LitNumber (PyInt "1")
                                       ])
    )
  , ( "p@{'metrics': {'latency': {service1: 10.0, 'OtherService': lat}}}"
    , NamedVpm (Identifier "p") $ VpmLiteral (LitDict [ ( LitString (PyUnicodeString "'metrics'")
                            , LitDict [ ( LitString (PyUnicodeString "'latency'")
                                        , LitDict [ ( LitId (Identifier "service1")
                                                    , LitNumber (PyFloat "10.0")
                                                    )
                                                  , ( LitString (PyUnicodeString "'OtherService'")
                                                    , LitId (Identifier "lat")
                                                    )
                                                  ]
                                        )
                                      ]
                            )
                          ])
    )
  , ( "3:T" -- this is semantically incorrect
            -- but it will be checked in another pass where semantics is validated
    , UnnamedVpm $ VpmSnoc (LitNumber (PyInt "3")) (LitBool PypesTrue)
    )
  , ( "_" , (UnnamedVpm VpmElse) )
  ]

rejectCases :: [Text]
rejectCases =
  [ "x@y@z:[1]"
  ]

spec :: Spec
spec = do
  describe "Variable pattern match parser" $ do
    forM_ acceptCases $ \(input, expected) -> assertTokenParserAccepts testParser (const expected) input
    forM_ rejectCases (assertTokenParserRejects testParser)

