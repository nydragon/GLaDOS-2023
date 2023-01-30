module CptTests where

import Test.Tasty
import Test.Tasty.HUnit

import Parsing.Token
import Parsing.Cpt

tokenToCptTests :: TestTree
tokenToCptTests = testGroup "tokenToCpt tests" [
    testCase "Token OpenScope returns Nothing" (assertEqual [] Nothing (tokenToCpt OpenScope)),
    testCase "Token CloseScope returns Nothing" (assertEqual [] Nothing (tokenToCpt CloseScope)),
    testCase "Token Num returns Just Val" (assertEqual [] (Just (Val 4)) (tokenToCpt (Num 4))),
    testCase "Token Negative Num returns Just Negative Val" (assertEqual [] (Just (Val (-4))) (tokenToCpt (Num (-4)))),
    testCase "Token Keyword returns Just Sym" (assertEqual [] (Just (Sym "test")) (tokenToCpt (Keyword "test")))
    ]

parseTokenListTests :: TestTree
parseTokenListTests = testGroup "Tests for parseTokenList function" [
    testCase "Empty input list" $
        parseTokenList [] @?= [],
    testCase "Input list starting with CloseScope" $
        parseTokenList [CloseScope] @?= [],
    testCase "Input list starting with OpenScope" $
        parseTokenList [OpenScope, Num 5, CloseScope] @?= [List [Val 5]],
    testCase "Input list with multiple nested scopes" $
        parseTokenList [OpenScope, Num 5, OpenScope, Num 3, CloseScope, CloseScope] @?= [List [Val 5, List [Val 3]]],
    testCase "Input list with multiple nested scopes and Keywords" $
        parseTokenList [OpenScope, Num 5, OpenScope, Keyword "add", Num 3, CloseScope, CloseScope] @?= [List [Val 5, List [Sym "add", Val 3]]],
    testCase "Input list with missing closing parenthesis" $
        parseTokenList [OpenScope, Num 5, OpenScope, Num 3] @?= [List [Val 5, List [Val 3]]]
    ]

-- Test cases for getCloseScope
getCloseScopeTests :: TestTree
getCloseScopeTests = testGroup "getCloseScope Tests" [
    testCase "Single parenthesis" $
       getCloseScope [OpenScope, CloseScope] @?= 1,
    testCase "Multiple parenthesis" $
        getCloseScope [OpenScope, OpenScope, CloseScope, CloseScope] @?= 3,
    testCase "Mismatched parenthesis 1" $
        getCloseScope [OpenScope, OpenScope, OpenScope, CloseScope, CloseScope, CloseScope, OpenScope, CloseScope] @?= 5,
    testCase "Mismatched parenthesis 2" $
        getCloseScope [OpenScope, OpenScope, CloseScope, OpenScope, CloseScope, CloseScope] @?= 5,
    testCase "Incomplete parenthesis" $
        getCloseScope [OpenScope, OpenScope, CloseScope] @?= 2,
    testCase "Empty list" $
        getCloseScope [] @?= 0
    ]

utilityFunctionsT :: TestTree
utilityFunctionsT = testGroup "Utility Functions Tests" [
    testCase "getSymbol with symbol string" (assertEqual [] (Just "abc") (getSymbol $ Sym "abc")),
    testCase "getSymbol with non-symbol string" (assertEqual [] Nothing (getSymbol $ Val 123)),
    testCase "getInteger with integer value" (assertEqual [] (Just 123) (getInteger $ Val 123)),
    testCase "getInteger with non-integer value" (assertEqual [] Nothing (getInteger $ Sym "abc")),
    testCase "getList with list value" (assertEqual [] (Just [Val 1, Sym "abc", Val 2]) (getList $ List [Val 1, Sym "abc", Val 2])),
    testCase "getList with non-list value" (assertEqual [] Nothing (getList $ Val 123))
    ]

-- Test suite list
cptSuite :: TestTree
cptSuite = testGroup "Parsing.Cpt Test Suite" [
        tokenToCptTests,
        getCloseScopeTests,
        parseTokenListTests
    ]