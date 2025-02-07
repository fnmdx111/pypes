module Compiler.Token.PyString () where

-- import Data.List (intercalate)
-- import Text.Megaparsec
-- import Text.Megaparsec.Char


-- data PyString =
--   PyStdString String
--   | PyRawString String
--   | PyFmtString String deriving (Show, Eq)


-- -- stdString :: Parser PyString
-- -- stdString = do
-- --   return . PyStdString $ ""

-- -- rawStringChar :: Parser String
-- -- rawStringChar = pure "1"
  

-- -- rawString :: Parser PyString
-- -- rawString = do
-- --   _ <- char 'r'
-- --   let quoteParser = choice [string "'''", (string ['"', '"', '"']), string "'", string "\""]
-- --   quote <- (parserTraced "quote" quoteParser)
-- --   content <- fmap concat (parserTraced "content" $ many rawStringChar)
-- --   _ <- string quote
-- --   return . PyRawString $ "r" ++ quote ++ content ++ quote


-- pyString :: Parser PyString
-- -- pyString = try rawString <|> stdString
-- pyString = char 'a'

