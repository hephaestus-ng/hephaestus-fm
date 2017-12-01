module Test.FM.TreeTest where

import Test.HUnit

import Control.Lens
import Data.Tree
import Data.Tree.Lens

import Data.FM.Feature
import Data.FM.Tree



------------------- FEATURE TREE - TEST 01 -------------------

ft01 = Node (Feature "iris" BasicFeature Mandatory) [
             (Node (Feature "security" OrFeature Mandatory) [
                (Node (Feature "sha-256" BasicFeature Optional) []),
                (Node (Feature "RSA" BasicFeature Optional) [])
             ]),
             (Node (Feature "persist" AltFeature Mandatory) [
                (Node (Feature "SQL" BasicFeature Optional) []),
                (Node (Feature "NoSQL" BasicFeature Optional) [])
             ])
            ]






------------------- FEATURE TREE - TEST 02 -------------------

ft02 = Node (Feature "iris" BasicFeature Mandatory) [
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
