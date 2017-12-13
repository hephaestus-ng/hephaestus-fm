module Data.FM.FeatureModelTest where

import Test.HUnit

import Data.Tree

import Data.FM.FeatureModel
import Data.FM.Expression
import Data.FM.Feature
import Data.FM.Tree
import Data.FM.SAT


ft01 = Node (Feature "iris" BasicFeature Mandatory) []

ft02 = Node (Feature "iris" BasicFeature Mandatory) [
        (Node (Feature "security" OrFeature Mandatory) []),
        (Node (Feature "persist" AltFeature Mandatory) [])
    ]

ft03 = Node (Feature "iris" BasicFeature Mandatory) [
             (Node (Feature "security" OrFeature Mandatory) [
                (Node (Feature "sha-256" BasicFeature Optional) []),
                (Node (Feature "RSA" BasicFeature Optional) [])
             ]),
             (Node (Feature "persist" AltFeature Mandatory) [
                (Node (Feature "SQL" BasicFeature Optional) []),
                (Node (Feature "NoSQL" BasicFeature Optional) [])
             ])
            ]

ft04 = Node (Feature "iris" BasicFeature Mandatory) [
         (Node (Feature "security" OrFeature Mandatory) [
            (Node (Feature "sha-256" BasicFeature Mandatory) []),
            (Node (Feature "RSA" BasicFeature Mandatory) [])
         ]),
         (Node (Feature "persist" AltFeature Mandatory) [
            (Node (Feature "SQL" AltFeature Mandatory) [
              (Node (Feature "PostgreSQL" BasicFeature Optional) []),
              (Node (Feature "MySQL" BasicFeature Optional) []),
              (Node (Feature "LiteSQL" BasicFeature Optional) [])
            ]),
            (Node (Feature "NoSQL" BasicFeature Mandatory) [
              (Node (Feature "MongoDB" BasicFeature Mandatory) [])
            ])
         ])
        ]
--
--
-- Expressions

ft01expr = fmToFeatureExpressions $ FeatureModel ft01 []
ft02expr = fmToFeatureExpressions $ FeatureModel ft02 []
ft03expr = fmToFeatureExpressions $ FeatureModel ft03 []
ft04expr = fmToFeatureExpressions $ FeatureModel ft04 []

test01expr = TestCase (assertEqual "FM01 Expressions test"
                        ([Ref "iris"]) ft01expr)

test02expr = TestCase (assertEqual "FM02 Expressions test"
            ([
            And (Or (Not (Ref "iris")) (Ref "security")) (Or (Ref "iris") (Not (Ref "security"))), -- iris <=> security
            And (Or (Not (Ref "iris")) (Ref "persist")) (Or (Ref "iris") (Not (Ref "persist"))),   -- iris <=> persist
            Ref "iris"]) ft02expr)                                                                 -- iris

test03expr = TestCase (assertEqual "FM03 Expressions test"
            ([

            Ref "iris"]) ft03expr)

--
--
-- SAT Solver

fm01 = FeatureModel ft01 []
fm02 = FeatureModel ft02 [Not (Ref "iris")]
fm03 = FeatureModel ft03 [Not (Ref "security")]
fm04 = FeatureModel ft04 [Not (Ref "persist")]

test01 = TestCase (assertEqual "SAT - satisfiable"
                  True (satSolver fm01))

test02 = TestCase (assertEqual "SAT - unsatisfiable"
                  False (satSolver fm02))

test03 = TestCase (assertEqual "SAT - unsatisfiable"
                  False (satSolver fm03))

test04 = TestCase (assertEqual "SAT - unsatisfiable"
                  False (satSolver fm04))


fm01Tests = TestList
      [
        TestLabel "SAT solving 1" test01,
        TestLabel "SAT solving 2" test02,
        TestLabel "SAT solving 3" test03,
        TestLabel "SAT solving 4" test04
      ]


runFeatureModelTests = runTestTT fm01Tests
