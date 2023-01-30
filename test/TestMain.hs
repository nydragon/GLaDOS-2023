import Data.List
import ParseArgsTest
import CptTests
import AstTests

import Test.Tasty
import Test.Tasty.Discover
import Test.Tasty.HUnit

{-# OPTIONS_GHC -F -pgmF tasty-discover#-}

main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "Glados Unit Tests" [
        tokenSuite,
        argsSuite,
        astSuite
    ]