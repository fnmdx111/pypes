{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Compiler.Program.Program
import Text.Megaparsec
import Data.Text
import Text.Show.Pretty (ppShow)
import Control.Monad

testPrograms :: [Text]
testPrograms =
  [ "a (= 2) b |> c d |> filter (2 !=)"
  , "x |> y |> z"
  , "x |> p@{}$ y; z$ w"
  , "x 1 2"
  , "x (y 1) 2 3"
  , "map (+ 9)"
  , "(y 1) x 2"
  , "(y |> x) 1 2"
  , "(a b) a b"
  ]

printOneProgram :: Text -> IO ()
printOneProgram input = let ast = parse pypesProgramP "" input
  in putStrLn $ ppShow ast

main :: IO ()
main = (forM_) testPrograms printOneProgram
