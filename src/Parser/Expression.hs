module Parser.Expression where

import Text.Parsec
import Text.Parsec.String

import Data.FM.Types
import Data.FM.Expression


parseExpression :: Parsec String () FeatureExp
parseExpression =
  parseExpNot   <|>
  parseExpAnd   <|>
  parseExpOr    <|>
  parseExpTrue  <|>
  parseExpFalse <|>
  parseExpRef


parseExpNot :: Parsec String () FeatureExp
parseExpNot =
  string "Not(" >> parseExpression >>= \e1 -> string ")" >> return (Not e1)

parseExpAnd :: Parsec String () FeatureExp
parseExpAnd =
  string "And(" >> parseExpression >>= \e1 -> many space >> string ","
  >> many space >> parseExpression >>= \e2 -> string ")" >>
  return (And e1 e2)

parseExpOr :: Parsec String () FeatureExp
parseExpOr =
  string "Or("  >> parseExpression >>= \e1 -> many space >> string ","
  >> many space >> parseExpression >>= \e2 -> string ")" >>
  return (Or e1 e2)

parseExpTrue :: Parsec String () FeatureExp
parseExpTrue =
  string "True" >> return (B True)

parseExpFalse :: Parsec String () FeatureExp
parseExpFalse =
  string "False" >> return (B False)

parseExpRef :: Parsec String () FeatureExp
parseExpRef =
  letter >> many (alphaNum <|> space) >>= \r -> return (Ref r)
