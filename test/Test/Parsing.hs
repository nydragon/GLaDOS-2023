module Test.Parsing where

import Test.Tasty
import Test.Tasty.Discover
import Test.Tasty.HUnit

import Test.Parsing.Args
import Test.Parsing.Lang

parsingTests :: TestTree
parsingTests = testGroup "Parsing Tests" [
    testLambda,
    testLambda'
    ]
