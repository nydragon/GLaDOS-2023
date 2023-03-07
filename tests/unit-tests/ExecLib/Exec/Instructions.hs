module ExecLib.Exec.Instructions where

import Test.Tasty
import Test.Tasty.HUnit

import Exec.Instructions
import qualified Exec.InferType as Type
import Exec.InferType
import qualified Data.Map as Map
import Data.Map (fromList)

-- pushValTest :: 

pushValSuite :: TestTree
pushValSuite = testGroup "pushVal" [
        testCase "push 3" $ pushVal (Type.Integer 3) initStack @?= Stack [] [Type.Integer 3] Map.empty,
        testCase "push \"Hello\"" $ pushVal (Type.String "\"Hello\"") initStack @?= Stack [] [Type.String "\"Hello\""] Map.empty
    ]

testPopVal :: Type -> ArgStack -> Assertion
testPopVal key val = localVars @?= fromList [(key, head val)]
    where (Stack [StackFrame localVars _ _] argsStack reg) = popVal key (Stack [makeStackFrame "test" 0] val Map.empty)

popValSuite :: TestTree 
popValSuite = testGroup "popVal" [
        testCase "pop" $ testPopVal (Type.Symbol "a") [Type.String "\"Hello\""],
        testCase "pop with remainers" $ testPopVal (Type.Symbol "a") [Type.Integer 3, Type.String "\"Hello\""]
    ]

instructionsSuite :: TestTree
instructionsSuite = testGroup "Exec.Infer TestSuite" [
        popValSuite,
        pushValSuite
    ]