module ExecTests where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map

import qualified Parsing.Ast as Ast
import Exec

-- Test data
vars = (Map.empty, Map.empty)
newVar = (Map.fromList [("x", Val 1)], Map.empty)
newFunc = (Map.empty, Map.fromList [("f", Ast.ExprList [Ast.Num 1])])

testLookupVar :: TestTree
testLookupVar = testGroup "Test lookupVar"
    [
        testCase "Returns the value of a defined variable" $
            (lookupVar "x" $ Map.fromList [("x", Val 3)]) @?= (Just $ Val 3),
        testCase "Returns Nothing if variable is not defined" $
            (lookupVar "y" $ Map.fromList [("x", Val 3)]) @?= Nothing
    ]

testLookupFunc :: TestTree
testLookupFunc = testGroup "Test lookupFunc"
    [
        testCase "Returns the body of a defined function" $
            (lookupFunc "add" $ Map.fromList [("add", Ast.ExprList [Ast.Symbole "+", Ast.Num 1, Ast.Num 2])])
                @?= (Just $ Ast.ExprList [Ast.Symbole "+", Ast.Num 1, Ast.Num 2]),
        testCase "Returns Nothing if function is not defined" $
            (lookupFunc "subtract" $ Map.fromList [("add", Ast.ExprList [Ast.Symbole "+", Ast.Num 1, Ast.Num 2])])
                @?= Nothing
    ]

testIsNameDefined :: TestTree
testIsNameDefined = testGroup "Test isNameDefined"
    [
        testCase "Returns True if name is defined as a variable" $
            (isNameDefined "x" (Map.fromList [("x", Val 3)], Map.fromList [])) @?= True,
        testCase "Returns True if name is defined as a function" $
            (isNameDefined "add" (Map.fromList [], Map.fromList [("add", Ast.ExprList [Ast.Symbole "+", Ast.Num 1, Ast.Num 2])])) @?= True,
        testCase "Returns False if name is not defined" $
            (isNameDefined "y" (Map.fromList [("x", Val 3)], Map.fromList [])) @?= False
    ]

-- Tests for defineVar
testDefineVar :: TestTree
testDefineVar = testGroup "defineVar tests"
  [ testCase "Should return Nothing if name already exists" $
      Nothing @=? defineVar "x" (Val 1) newVar
  , testCase "Should add variable if name doesn't exist" $
      Just newVar @=? defineVar "x" (Val 1) vars
  ]

-- Tests for defineFunc
testDefineFunc :: TestTree
testDefineFunc = testGroup "defineFunc tests"
  [ testCase "Should return Nothing if name already exists" $
      Nothing @=? defineFunc "f" (Ast.ExprList [Ast.Num 1]) newFunc
  , testCase "Should return Nothing if body is not ExprList" $
      Nothing @=? defineFunc "f" (Ast.Num 1) vars
  , testCase "Should add function if name doesn't exist and body is ExprList" $
      Just newFunc @=? defineFunc "f" (Ast.ExprList [Ast.Num 1]) vars
  ]

execSuite :: TestTree
execSuite = testGroup "Execution Suite Tests" [
        testIsNameDefined,
        testLookupVar,
        testLookupFunc,
        testDefineVar,
        testDefineFunc
    ]