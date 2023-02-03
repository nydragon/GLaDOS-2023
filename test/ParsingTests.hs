module ParsingTests where

import Parsing.CptTests
import Parsing.ArgsTests
import Parsing.AstTests
import Parsing.TokenTests

import Test.Tasty
import Test.Tasty.HUnit

parsingSuite :: TestTree
parsingSuite = testGroup "Parsing Suite Tests" [
        argsSuite,
        tokenSuite,
        cptSuite,
        astSuite
    ]