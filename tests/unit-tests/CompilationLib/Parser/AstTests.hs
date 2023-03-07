module CompilationLib.Parser.AstTests where

import Test.Tasty
import Test.Tasty.HUnit

import Parsing.Ast
import qualified Parsing.Cpt as Cpt

-- Input values
singleVal = [Cpt.Val 1]
singleSym = [Cpt.Sym "test"]
variedList = [Cpt.Sym "test", Cpt.Val 1, Cpt.Val 2]
singleCall = [Cpt.List [Cpt.Sym "+", Cpt.Val 1, Cpt.Val 2]]

-- Note: We don't care that these are invalid arguments at this stage of testing
listWithCall = [Cpt.Sym "+", Cpt.Val 1, Cpt.List [Cpt.Sym "*", Cpt.Boolean True, Cpt.Val 3]] -- is this valid
nestedList = [Cpt.List [Cpt.Sym "test", Cpt.Val 1, Cpt.Val 2]]
nestedCall = [Cpt.List [Cpt.Sym "test", Cpt.List [Cpt.Sym "+", Cpt.Val 1, Cpt.Val 2]]]

-- Assertion values
-- (What we test against)
singleValAssert = [Num 1]
singleSymAssert = [Symbole "test"]
variedListAssert = [Symbole "test", Num 1, Num 2]
singleCallAssert = [Call "+" [Num 1, Num 2]]
listWithCallAssert = [Symbole "+",Num 1,Call "*" [Boolean True,Num 3]]
nestedListAssert = ExprList [ExprList [Symbole "test", Num 1, Num 2]]
nestedCallAssert = ExprList [ExprList [Symbole "test", Call "+" [Num 1, Num 2]]]

parseExprListTests :: TestTree
parseExprListTests = testGroup "Tests for parseExprList" [
    testCase "Parse empty expr list" $
        assertEqual "Lists are empty" (parseExprList []) [],
    testCase "Parse expr list with single Val" $
        parseExprList singleVal @?= singleValAssert,
    testCase "Parse expr list with single Sym" $
        parseExprList singleSym @?= singleSymAssert,
    testCase "Parse expr list with multiple atom elements" $
        parseExprList variedList @?= variedListAssert,
    testCase "Parse expr list with single funtion call" $
        parseExprList singleCall @?= singleCallAssert,
    testCase "Parse expr list with nested call and multiple elements" $
        parseExprList listWithCall @?= listWithCallAssert
    ]

parseExprTests :: TestTree
parseExprTests = testGroup "Tests for parseExpr" [
    testCase "Parse input list with single Sym" $
        parseExpr singleSym @?= ExprList singleSymAssert,
    testCase "Parse input list with multiple elements" $
        parseExpr variedList @?= ExprList variedListAssert,
    testCase "Parse input list with nested list" $
        parseExpr nestedList @?= nestedListAssert,
    testCase "Parse input list with nested call" $
        parseExpr nestedCall @?= nestedCallAssert
    ]

exprListToCallTests :: TestTree
exprListToCallTests = testGroup "Tests for exprListToCall function" [
    testCase "Input list is empty" $
        exprListToCall [] @?= Nothing,
    testCase "Input list starts with a Symbole" $
        exprListToCall [Symbole "add", Num 3, Num 4] @?= Just (Call "add" [Num 3, Num 4]),
    testCase "Input list does not start with a Symbole" $
        exprListToCall [Num 3, Num 4] @?= Nothing
    ]


astSuite :: TestTree
astSuite = testGroup "Parsing.Ast Test Suite" [
        parseExprListTests,
        parseExprTests,
        exprListToCallTests
    ]