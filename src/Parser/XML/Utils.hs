module Parser.XML.Utils where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim


blanks :: Parsec String () String
blanks = many (space <|> newline)

endTag :: Parsec String () String
endTag = ((string "/>") <|> (string ">")) >> blanks
