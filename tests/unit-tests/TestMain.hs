import Test.Tasty
import Test.Tasty.Discover
import Test.Tasty.HUnit

import ExecLib
import CompilationLib

{-# OPTIONS_GHC -F -pgmF tasty-discover#-}

main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "Glados Unit Tests" [
        execLibSuite,
        compilationLibSuite
    ]