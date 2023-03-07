module ExecLib where

import Test.Tasty
import Test.Tasty.HUnit

import ExecLib.Exec.Infer ( inferSuite )
import ExecLib.Exec.Instructions ( instructionsSuite )
import ExecLib.Exec.Utils ( utilsSuite )
import ExecLib.Parser.ReadFile (readFileSuite)

execLibSuite :: TestTree
execLibSuite = testGroup "Exec TestSuite" [
        inferSuite,
        instructionsSuite,
        utilsSuite,
        readFileSuite
    ]