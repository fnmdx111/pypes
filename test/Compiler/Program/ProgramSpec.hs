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
  ]

rejectCases :: [Text]
rejectCases =
  [ "x |> y |>"
  ]

spec :: Spec
spec = do
  describe "Variable pattern match parser" $ do
    forM_ acceptCases $ \(input, expected) -> assertTokenParserAccepts testParser (const expected) input
    forM_ rejectCases (assertTokenParserRejects testParser)
