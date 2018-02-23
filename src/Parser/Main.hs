module Parser.Main where

import Parser.Parser
import Parser.Operators
import Parser.Transformers


-- yields a type Parser Char (Char, (Char, Char)) that can be applied to
-- a given list xs
p = symbol 'a' <**> symbol 'b' <**> symbol 'c'
