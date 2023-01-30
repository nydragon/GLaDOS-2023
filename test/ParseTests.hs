module ParseTests where

import Test.Tasty
import Test.Tasty.HUnit

import Parse.ArgsTests
import Parse.CptTests
import Parse.TokenTests


parseSuite :: TestTree
parseSuite = testGroup "Parse Tests Suite" [
        argsSuite,
        tokenSuite,
        cptSuite
    ]