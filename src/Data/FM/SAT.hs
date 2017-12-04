module Data.FM.SAT (satSolver) where

-- Module for SAT solving our FM propositional logic expressions

import Data.FM.Feature
import Data.FM.Tree
import Data.FM.Expression
import Data.FM.FeatureModel

import Control.Applicative ((<|>))



-- Basic algorithm by backtracking search, following these steps:
-- 1. Find a variable in the constraint expression that hasn't been assigned (a free variable).
-- 2. Guess a value for this free variable.
-- 3. Replace all occurrences of the free variable with the guessed value.
-- 4. Simplify the expression. If the expression simplifies to true (one), then the values we've assigned work,
-- and any variables that are unassigned do not matter. If the expression simplifies to false (zero),
-- then undo the last assignment, and assign the opposite value.


-- <|> is used to choose the first Just we encounter
findFreeVariable :: FeatureExp -> Maybe String
findFreeVariable (B _)     = Nothing
findFreeVariable (Ref s)   = Just s
findFreeVariable (Not p)   = findFreeVariable p
findFreeVariable (And p q) = findFreeVariable p <|> findFreeVariable q
findFreeVariable (Or p q)  = findFreeVariable p <|> findFreeVariable q


guessVariable :: String -> Bool -> FeatureExp -> FeatureExp
guessVariable ref val expr = case expr of
    Ref s   -> if s == ref then B val
               else Ref s
    Not p   -> Not (guess p)
    And p q -> And (guess p) (guess q)
    Or p q  -> Or (guess p) (guess q)
    B b     -> B b
  where
    guess = guessVariable ref val


-- COPY OF THE ARTICLE TEXT
-- Thus, we introduce simplify, which returns either a Const constructor or a simplified expression;
-- if the result is not a Const constructor, it guarantees that there are no Const constructors in the Expr tree further down.
-- (Note that we could encode this invariant in the type system! The best way to do this is left as an exercise to the reader.)
simplify :: FeatureExp -> FeatureExp
simplify (B b)    = B b
simplify (Ref s)  = Ref s
simplify (Not p)  = case simplify p of
    B b -> B (not b)
    p   -> Not p
simplify (Or p q) =
    let
      es = filter (/= B False) [simplify p, simplify q]
    in
      if B True `elem` es then B True
      else
        case es of
          []       -> B False
          [e]      -> e
          [e1, e2] -> Or e1 e2
simplify (And p q) =
    let
      es = filter (/= B True) [simplify p, simplify q]
    in
      if B False `elem` es then B False
      else
        case es of
          []       -> B True
          [e]      -> e
          [e1, e2] -> And e1 e2


extractBool :: FeatureExp -> Bool
extractBool (B b)     = b
extractBool (Not p)   = not (extractBool p)
extractBool (And p q) = (extractBool p) && (extractBool q)
extractBool (Or p q)  = (extractBool p) || (extractBool q)
extractBool _         = error "Not a boolean value"


satisfiable :: FeatureExp -> Bool
satisfiable expr = case findFreeVariable expr of
    Nothing -> extractBool expr
    Just r  ->
      let
        trueGuess  = simplify (guessVariable r True expr)
        falseGuess = simplify (guessVariable r False expr)
      in
        satisfiable trueGuess || satisfiable falseGuess


satSolver :: FeatureModel -> Bool
satSolver fm =
    let
      expr = foldr And (B True) (fmToFeatureExpressions fm)
    in
      satisfiable expr
