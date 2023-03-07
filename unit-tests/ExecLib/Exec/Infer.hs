module ExecLib.Exec.Infer where

import Test.Tasty
import Test.Tasty.HUnit

import Exec.Infer
import Exec.InferType
import qualified Exec.InferType as Type

testIsSymbol :: TestTree
testIsSymbol = testGroup "isSymbol" [
        testCase "is not a symbol" $ isSymbol "\"Hello World\"" @?= False,
        testCase "is a symbol" $ isSymbol "Symbol" @?= True
    ]

testIsString :: TestTree
testIsString = testGroup "isString" [
        testCase "is a string" $ isString "\"Hello World\"" @?= True,
        testCase "is not a string" $ isString "Symbol" @?= False
    ]

testInfer :: TestTree
testInfer = testGroup "infer" [
        testCase "is an integer" $ infer "4" @?= Type.Integer 4,
        testCase "is a float" $ infer "4.5" @?= Type.Float 4.5,
        testCase "is a string" $ infer "\"Hello World\"" @?= Type.String "\"Hello World\"",
        testCase "is a symbol " $ infer "Symbol" @?= Type.Symbol "Symbol",
        testCase "is a 2 element array" $ infer "(1,3)" @?= Type.List [Type.Integer 1, Type.Integer 3],
        testCase "is an empty array" $ infer "()" @?= Type.List [],
        testCase "is a 2d array" $ infer "((1,2),(3,4))" @?= Type.List [Type.List [Type.Integer 1, Type.Integer 2], Type.List [Type.Integer 3, Type.Integer 4]],
        testCase "is a complex array" $ infer "(((1,2),(1,2)),(3,4))" @?= Type.List [Type.List [Type.List [Type.Integer 1, Type.Integer 2], Type.List [Type.Integer 1, Type.Integer 2]], Type.List [Type.Integer 3, Type.Integer 4]],
        testCase "is a hyper complex array" $ infer "(((1,2),(1,2)),((1,2),(1,2)))" @?= Type.List [Type.List [Type.List [Type.Integer 1, Type.Integer 2], Type.List [Type.Integer 1, Type.Integer 2]], Type.List [Type.List [Type.Integer 1, Type.Integer 2], Type.List [Type.Integer 1, Type.Integer 2]]]
    ]

inferSuite :: TestTree
inferSuite = testGroup "Exec.Infer TestSuite" [
        testIsString,
        testIsSymbol,
        testInfer
    ]