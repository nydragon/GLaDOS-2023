module ParsingTests where

import Parsing.ArgsTests
import Parsing.AstTests
import Parsing.CptTests
import Parsing.TokenTests
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

parsingSuite :: TestTree
parsingSuite =
  testGroup
    "Parsing Suite Tests"
    [ argsSuite,
      tokenSuite,
      cptSuite,
      astSuite
    ]