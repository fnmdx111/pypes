{-# LANGUAGE OverloadedStrings #-}

module Compiler.Token.Bool (PypesBool(..), boolT) where
import Compiler.Token.Util (Parser, chunkT)
import Control.Applicative (asum)
import Text.Megaparsec (try)

data PypesBool = PypesTrue | PypesFalse deriving (Show, Eq)

boolT :: Parser PypesBool
boolT = asum [ try $ chunkT "True" >> pure PypesTrue
             , try $ chunkT "False" >> pure PypesFalse
             , try $ chunkT "T" >> pure PypesTrue
             , chunkT "F" >> pure PypesFalse
             ]
