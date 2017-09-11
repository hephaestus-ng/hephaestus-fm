module Data.FM.Expression where

-- Module to convert a feature model derivation to propositional logic expression

import Test.HUnit
import Data.FM.Tree

data FeatureExp = B Bool
                | Ref String
                | And FeatureExp FeatureExp
                | Or FeatureExp FeatureExp
                | Not FeatureExp
                -- | Tuple (FeatureExp, [FeatureExp])
    deriving(Show)


eval :: FeatureExp -> Bool
eval (B b)           = b
eval (Ref s)         = True
eval (And exp1 exp2) = (eval exp1) && (eval exp2)
eval (Or exp1 exp2)  = (eval exp1) || (eval exp2)
eval (Not exp1)      = not (eval exp1)


-- todo: convert feature tree to a propositional logic expression list, for validation
featureTreeToExp :: FeatureTree -> [FeatureExp]
featureTreeToExp _ = []


(.=>) :: FeatureExp -> FeatureExp -> FeatureExp
p .=> q = Or (Not p) q

(<=>) :: FeatureExp -> FeatureExp -> FeatureExp
p <=> q = And (Or (Not p) q) (Or p (Not q))

-- (/\) :: FeatureExp -> FeatureExp -> FeatureExp
-- p /\ q = And p q
--
-- (\/) :: FeatureExp -> FeatureExp -> FeatureExp
-- p \/ q = Or p q


choose :: [FeatureExp] -> Int -> FeatureExp
choose fs n = fs !! n

chooseMany :: [FeatureExp] -> Int -> FeatureExp
chooseMany fs 0 = B True
chooseMany fs n = And (choose fs n) (chooseMany fs (n-1))


-- tc01 = TestCase (assertEqual "test for exp eval" (eval $ (B True) <=> (B True) /\ (B False) .=> (B True)) True)



-- ["iris" <=> "persistence", (Or $ "nosql" => "persistence" "sql" => "persistence") ,]
