module Parser.Operators where

import Parser.Parser


(<**>) :: Parser s a -> Parser s b -> Parser s (a, b)
(p1 <**> p2) xs = [ (xs2, (v1, v2)) | (xs1, v1) <- p1 xs,
                                      (xs2, v2) <- p2 xs1 ]


(<|>) :: Parser s a -> Parser s a -> Parser s a
(p1 <|> p2) xs = p1 xs ++ p2 xs
