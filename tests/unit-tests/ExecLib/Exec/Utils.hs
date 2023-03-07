module ExecLib.Exec.Utils where

import Test.Tasty
import Test.Tasty.HUnit

import Exec.Infer
import Exec.InferType
import qualified Exec.InferType as Type
import Exec.Utils (parseNum, assignRet, lookupRet)
import qualified Data.Map as Map
import Data.Map (fromList)

testParseNum :: TestTree
testParseNum = testGroup "parseNum" [
        testCase "parse pos integer" $ parseNum "4" @?= Type.Integer 4,
        testCase "parse neg integer" $ parseNum "-4" @?= Type.Integer (-4),
        testCase "parse pos float" $ parseNum "4.2" @?= Type.Float 4.2,
        testCase "parse neg float" $ parseNum "-4.2" @?= Type.Float (-4.2)
    ]

makeReg :: Type -> Registers
makeReg val = fromList [(Type.Symbol "#RET", val)]

testAssignRet :: TestTree
testAssignRet = testGroup "testAssignRet" [
        testCase "assign ret" $ assignRet (Type.Integer 3) Map.empty @?= makeReg (Type.Integer 3),
        testCase "assign ret" $ assignRet (Type.String "\"Hello\"") Map.empty @?= makeReg (Type.String "\"Hello\"")
    ]

testLookupRet :: TestTree
testLookupRet = testGroup "testLookupRet" [
        testCase "lookup ret" $ lookupRet (makeReg (Type.Integer 3))  @?= Just (Type.Integer 3),
        testCase "lookup ret" $ lookupRet (makeReg (Type.String "\"Hello\""))  @?= Just (Type.String "\"Hello\"")
    ]

utilsSuite :: TestTree
utilsSuite = testGroup "Exec.Infer TestSuite" [
        testParseNum,
        testAssignRet,
        testLookupRet
    ]