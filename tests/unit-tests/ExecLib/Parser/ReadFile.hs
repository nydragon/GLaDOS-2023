module ExecLib.Parser.ReadFile where

import Test.Tasty
import Test.Tasty.HUnit

import Instruction (Instruction(Push, Pop, Call, Init, Move))
import Parser.ReadFile (stringToInstruction)

stringToInstructionTest :: TestTree
stringToInstructionTest = testGroup "stringToInstruction" [
        testCase "parse push" $ stringToInstruction "push 3" @?= Push "3",
        testCase "parse pop" $ stringToInstruction "pop q" @?= Pop "q",
        testCase "parse call" $ stringToInstruction "call q" @?= Call "q",
        testCase "parse init" $ stringToInstruction "init q" @?= Init "q",
        testCase "parse move" $ stringToInstruction "move 3 q" @?= Move "3" "q",
        testCase "parse move" $ stringToInstruction "move \"Hello World\" q" @?= Move "\"Hello World\"" "q"
    ]

readFileSuite :: TestTree
readFileSuite = testGroup "Exec.ReadFile TestSuite" [
        stringToInstructionTest
    ]