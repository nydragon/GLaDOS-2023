module ExecTests where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map

import qualified Parsing.Ast as Ast
import Exec.Lookup

-- Test data
vars = (Map.empty, Map.empty)
newVar = (Map.fromList [("x", Ast.Num 1)], Map.empty)

testLookupVar :: TestTree
testLookupVar = testGroup "Test lookupVar"
    [
        testCase "Returns the value of a defined variable" $
            (lookupVar "x" $ Map.fromList [("x", Ast.Num 3)]) @?= (Just $ Ast.Num 3),
        testCase "Returns Nothing if variable is not defined" $
            (lookupVar "y" $ Map.fromList [("x", Ast.Num 3)]) @?= Nothing
    ]

-- Note : Will need to add tests to avoid passing integers as function names
testLookupFunc :: TestTree
testLookupFunc = testGroup "Test lookupFunc"
    [
        testCase "Returns the body of a defined function" $
            assertEqual "Incorrect body for 'add'" assert1
            (lookupFunc "add" input1),
        testCase "Returns Nothing if function is not defined" $
            assertEqual "Incorrect result for 'subtract'" Nothing
            (lookupFunc "subtract" Map.empty)
    ]
        where   input1 = Map.fromList [("add", (["1", "2"], Ast.ExprList [Ast.Symbole "+", Ast.Num 1, Ast.Num 2]))]
                assert1 = Just $ (["1", "2"], Ast.ExprList [Ast.Symbole "+", Ast.Num 1, Ast.Num 2])

testIsNameDefined :: TestTree
testIsNameDefined = testGroup "Test isNameDefined"
    [
        testCase "Returns True if name is defined as a variable" $
            isNameDefined "x" (Map.fromList [("x", Ast.Num 3)], Map.empty) @?= True,
        testCase "Returns True if name is defined as a function" $
            isNameDefined "add" input1 @?= True,
        testCase "Returns False if name is not defined" $
            isNameDefined "y" (Map.fromList [("x", Ast.Num 3)], Map.empty) @?= False
    ]
        where   input1 = (Map.empty, Map.fromList [("add", (["1", "2"], Ast.ExprList [Ast.Symbole "+", Ast.Num 1, Ast.Num 2]))])

-- Tests for defineVar
testDefineVar :: TestTree
testDefineVar = testGroup "defineVar tests"
  [ testCase "Should return Nothing if name already exists" $
      Nothing @=? defineVar "x" (Ast.Num 1) newVar
  , testCase "Should add variable if name doesn't exist" $
      Just newVar @=? defineVar "x" (Ast.Num 1) vars
  ]

-- Tests for defineFunc
testDefineFunc :: TestTree
testDefineFunc = testGroup "defineFunc tests"
  [ testCase "Should return Nothing if name already exists" $
      Nothing @=? defineFunc "add" ["a", "b"] (Ast.ExprList []) lookup
  , testCase "Should return Nothing if body is not ExprList" $
      Nothing @=? defineFunc "f" ["a", "b"] (Ast.Num 1) lookup
  , testCase "Should add function if name doesn't exist and body is ExprList" $
      Just expectedLookup @=? defineFunc "sub" ["a", "b"] (Ast.ExprList [Ast.Symbole "+", Ast.Num 1, Ast.Num 2]) lookup
  ]
    where   lookup = (Map.empty, Map.fromList [("add", (["1", "2"], Ast.ExprList [Ast.Symbole "+", Ast.Num 1, Ast.Num 2]))])
            expectedLookup = (Map.empty, Map.fromList [("sub", (["a", "b"], Ast.ExprList [Ast.Symbole "+", Ast.Num 1, Ast.Num 2])), ("add", (["1", "2"], Ast.ExprList [Ast.Symbole "+", Ast.Num 1, Ast.Num 2]))])

execSuite :: TestTree
execSuite = testGroup "Execution Suite Tests" [
        testIsNameDefined,
        testLookupVar,
        testLookupFunc,
        testDefineVar,
        testDefineFunc
    ]