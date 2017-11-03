module Test.FM.FeatureTest where

import Test.HUnit

import Data.FM.Feature
import Control.Lens


feature01 = Feature "Iris" BasicFeature Mandatory


test01 = TestCase (assertEqual "Testing name lens reference"  (view name feature01) "Iris")
test02 = TestCase (assertEqual "Testing group lens reference" (view group feature01) BasicFeature)
test03 = TestCase (assertEqual "Testing type lens reference"  (view typeF feature01) Mandatory)

tests = TestList
    [
        TestLabel "feature name"  test01,
        TestLabel "feature group" test02,
        TestLabel "feature type"  test03
    ]


runFeatureTests = runTestTT tests
