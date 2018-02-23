module Parser.Transformers where

import Parser.Parser

sp :: Parser Char a -> Parser Char a
sp p = p . dropWhile (== ' ')
