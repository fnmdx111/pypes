{-# LANGUAGE OverloadedStrings #-}
module Compiler.Token.None (PypesNone(..), noneT) where

import Compiler.Token.Util (Parser, chunkT)
import Control.Applicative (asum)
import Text.Megaparsec (try)

data PypesNone = PypesNone deriving (Show, Eq)

noneT :: Parser PypesNone
noneT = asum [ try $ chunkT "None" >> pure PypesNone
             , try (chunkT "Nil") >> pure PypesNone
             , (chunkT "Null") >> pure PypesNone
             ]
