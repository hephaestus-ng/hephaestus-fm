module Test.FM.ExpressionTest where

import Test.HUnit

import Data.FM.Expression
import Data.FM.Feature
import Data.FM.Tree
import Data.Tree

evalTest :: FeatureExp -> Bool
evalTest (B b)           = b
evalTest (And exp1 exp2) = (evalTest exp1) && (evalTest exp2)
evalTest (Or exp1 exp2)  = (evalTest exp1) || (evalTest exp2)
evalTest (Not exp1)      = not (evalTest exp1)

-- Tests for <=> operator
test01 = TestCase (assertEqual "Testing <=> operator (True <=> True)"
                  (evalTest $ (B True) <=> (B True)) True)
test02 = TestCase (assertEqual "Testing <=> operator (True <=> False)"
                  (evalTest $ (B True) <=> (B False)) False)
test03 = TestCase (assertEqual "Testing <=> operator (False <=> False)"
                  (evalTest $ (B False) <=> (B False)) True)
test04 = TestCase (assertEqual "Testing <=> operator (False <=> True)"
                  (evalTest $ (B False) <=> (B True)) False)

-- Tests for .=> operator
test05 = TestCase (assertEqual "Testing .=> operator (True .=> True)"
                  (evalTest $ (B True) .=> (B True)) True)
test06 = TestCase (assertEqual "Testing .=> operator (True .=> False)"
                  (evalTest $ (B True) .=> (B False)) False)
test07 = TestCase (assertEqual "Testing .=> operator (False .=> False)"
                  (evalTest $ (B False) .=> (B False)) True)
test08 = TestCase (assertEqual "Testing .=> operator (False .=> True)"
                  (evalTest $ (B False) .=> (B True)) True)


-- Tests for xor operator
test09 = TestCase (assertEqual "Testing xor operator (True xor True)"
                  (evalTest $ (B True) `xor` (B True)) False)
test10 = TestCase (assertEqual "Testing xor operator (True xor False)"
                  (evalTest $ (B True) `xor` (B False)) True)
test11 = TestCase (assertEqual "Testing xor operator (False xor False)"
                  (evalTest $ (B False) `xor` (B False)) False)
test12 = TestCase (assertEqual "Testing xor operator (False xor True)"
                  (evalTest $ (B False) `xor` (B True)) True)


operatorsTests = TestList
    [
        TestLabel "<=>" test01,
        TestLabel "<=>" test02,
        TestLabel "<=>" test03,
        TestLabel "<=>" test04,

        TestLabel ".=>" test05,
        TestLabel ".=>" test06,
        TestLabel ".=>" test07,
        TestLabel ".=>" test08,

        TestLabel "xor" test09,
        TestLabel "xor" test10,
        TestLabel "xor" test11,
        TestLabel "xor" test12
    ]

runOperatorsTests = runTestTT operatorsTests
