{-# LANGUAGE OverloadedStrings #-}

module Compiler.Program.ProgramSpec (spec) where

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
import Compiler.Program.Program (PypesProgram(..), PypesExpr(..), pypesProgramP)
import Compiler.Program.Program (BinOp(..))

testParser :: Parser PypesProgram
testParser = program pypesProgramP

acceptCases :: [(Text, PypesProgram)]
acceptCases =
  [ ( "x |> y |> z"
    , Expr (PipeExpr
            (PipeExpr
              (LitExpr (LitId (Identifier "x")))
              Nothing
              (LitExpr (LitId (Identifier "y"))))
             Nothing
             (LitExpr (LitId (Identifier "z"))))
    )
  , ( "x |p@{}> y |z> w"
    , Expr (PipeExpr
             (PipeExpr
               (LitExpr (LitId (Identifier "x")))
               (Just (NamedVpm (Identifier "p") (VpmLiteral (LitDict []))))
               (LitExpr (LitId (Identifier "y"))))
             (Just (UnnamedVpm (VpmLiteral (LitId (Identifier "z")))))
             (LitExpr (LitId (Identifier "w")))))
  , ( "x 1 2 $"
    , Expr (FunExpr
             (LitExpr (LitId (Identifier "x")))
             [ (LitExpr (LitNumber (PyInt "1")))
             , (LitExpr (LitNumber (PyInt "2")))])
    )
  , ( "x (y 1 $) 2 3 $"
    , Expr (FunExpr
             (LitExpr (LitId (Identifier "x")))
             [ (FunExpr
                 (LitExpr (LitId (Identifier "y")))
                 [ (LitExpr (LitNumber (PyInt "1"))) ])
             , (LitExpr (LitNumber (PyInt "2")))
             , (LitExpr (LitNumber (PyInt "3")))])
    )
  , ( "x (y 1) 2 3 $"
    , Expr (FunExpr
             (LitExpr (LitId (Identifier "x")))
             [ (FunExpr
                 (LitExpr (LitId (Identifier "y")))
                 [ (LitExpr (LitNumber (PyInt "1"))) ])
             , (LitExpr (LitNumber (PyInt "2")))
             , (LitExpr (LitNumber (PyInt "3")))])
    )
  , ( "map (+ 9) $"
    , Expr (FunExpr
             (LitExpr (LitId (Identifier "map")))
           [ (RightPartialBinaryOpFunExpr Add (LitExpr (LitNumber (PyInt "9"))))
           ]))
  , ( "(y 1) x 2 $"
    , Expr (FunExpr
             (FunExpr
               (LitExpr (LitId (Identifier "y")))
               [ (LitExpr (LitNumber (PyInt "1"))) ])
             [ (LitExpr (LitId (Identifier "x")))
             , (LitExpr (LitNumber (PyInt "2"))) ])
    )
  , ( "a b $ c d $"
    , Expr (FunExpr
             (FunExpr
               (LitExpr (LitId (Identifier "a")))
               [(LitExpr (LitId (Identifier "b")))])
             [ (LitExpr (LitId (Identifier "c")))
             , (LitExpr (LitId (Identifier "d"))) ])
    )
  ]

rejectCases :: [Text]
rejectCases =
  [ "x |> y |>"
  , "x 1 2"
  ]

spec :: Spec
spec = do
  describe "Variable pattern match parser" $ do
    forM_ acceptCases $ \(input, expected) -> assertTokenParserAccepts testParser (const expected) input
    forM_ rejectCases (assertTokenParserRejects testParser)
