import Data.List

import Test.Tasty
import Test.Tasty.Discover
import Test.Tasty.HUnit

import ParsingTests
import ExecTests

{-# OPTIONS_GHC -F -pgmF tasty-discover#-}

main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "Glados Unit Tests" [
        parsingSuite,
        execSuite
    ]