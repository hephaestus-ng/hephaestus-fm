module Parser.Parser (Parser) where


type Parser a b = [a] -> [ ([a], b) ]


token :: Eq s => [s] -> Parser s [s]
token k []            = []
token k xs
  | k == take n xs    = [ (drop n xs, k)]
  | otherwise         = []
      where n = length k


satisfy :: (s -> Bool) -> Parser s s
satisfy p []     = []
satisfy p (x:xs) = [ (xs, x) | p x]

epsilon :: Parser s ()
epsilon xs = [ (xs, ()) ] -- or -> epsilon = succeed ()

succeed :: r -> Parser s r
succeed v xs = [ (xs, v) ]

failure :: Parser s r
failure xs = []

symbol :: Eq a => a -> Parser a a
symbol s []     = []
symbol s (x:xs) = [ (xs, s) | s == x]


-- play

recsatisfy :: (s -> Bool) -> Parser s s
recsatisfy p []     = []
recsatisfy p (x:xs) = [ (xs, x) | p x] ++ recsatisfy p xs

equalsymbol :: Eq a => a -> Parser a a
equalsymbol p [] = []
equalsymbol p l  = satisfy (\z -> z == p) l

existsymbol :: Eq a => a -> Parser a a
existsymbol p [] = []
existsymbol p l  = recsatisfy (\z -> z == p) l
