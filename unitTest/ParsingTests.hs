module ParsingTests where

import Test.Tasty
import Test.Tasty.HUnit

import Parsing.CptTests
import Parsing.ArgsTests
import Parsing.AstTests
import Parsing.TokenTests

parsingSuite :: TestTree
parsingSuite = testGroup "Parsing Suite Tests" [
        argsSuite,
        tokenSuite,
        cptSuite,
        astSuite
    ]