import Test.Tasty
import Test.Tasty.Discover
import Test.Tasty.HUnit

import ExecLib

{-# OPTIONS_GHC -F -pgmF tasty-discover#-}

main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "Glados Unit Tests" [
        execLibSuite
    ]