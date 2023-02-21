module Parsing.Infix where

import qualified Parsing.Cpt as Cpt
import           Data.Maybe  (fromMaybe)
import           Debug.Trace (trace)

getPrecedence ::  [Cpt.Cpt] -> Integer
getPrecedence [] = 0
getPrecedence (a : _) = getOpPrecedence a

process :: [Cpt.Cpt] -> [Cpt.Cpt] -> [Cpt.Cpt] -> ([Cpt.Cpt], [Cpt.Cpt])
process buffer [] _ = (buffer, [])
process buffer opStack expr 
    | getOpPrecedence op >= precedence || null expr = process (buffer ++ [op]) (init opStack) expr
    | otherwise = (buffer, opStack)
        where
            op = last opStack
            precedence =  getPrecedence expr

-- when encountering an operator we pop the operator stack onto the value stack
-- as described in Djikstra's shunting yard algorithm
popStack :: [Cpt.Cpt] -> [Cpt.Cpt] -> [Cpt.Cpt] -> Integer -> [Cpt.Cpt] -> Maybe [Cpt.Cpt]
popStack infixExpr valStack opStack i buffer
    | null infixExpr = Just newBuffer
    | otherwise =  infixToPrefix'' infixExpr [] newOpStack i newBuffer
    where
        (newBuffer, newOpStack) = process (buffer ++ valStack) opStack infixExpr

-- An implementation of Djikstra's shunting yard algorithm
infixToPrefix'' :: [Cpt.Cpt] -> [Cpt.Cpt] -> [Cpt.Cpt] -> Integer -> [Cpt.Cpt] ->  Maybe [Cpt.Cpt]
infixToPrefix'' [x] valStack opStack i buffer = trace ("infixToPrefix'' 1" ++ show [x]) $ popStack [] (valStack <> [x]) opStack i buffer
infixToPrefix'' (x: xs) valStack opStack i buffer
    | even i = trace ("infixToPrefix'' 2" ++ show (x : xs) ++ show valStack ++ show opStack) $ infixToPrefix'' xs (valStack <> [x]) opStack (succ i) buffer
    | odd i && getOpPrecedence x > getOpPrecedence (last opStack) = trace ("infixToPrefix'' 3" ++ show (x : xs)) $ infixToPrefix'' xs valStack (opStack <> [x]) (succ i) buffer
    | otherwise = trace ("infixToPrefix'' 4" ++ show (x : xs)) $ popStack (x : xs) valStack opStack i buffer
infixToPrefix'' x valStack opStack i buffer = trace ("infixToPrefix'' 5" ++ show x) $ Just buffer

infixToPrefix' :: [Cpt.Cpt] -> [Cpt.Cpt]
infixToPrefix' a = trace ("infixToPrefix'" ++ show a) $ reverseList $ fromMaybe [] $ infixToPrefix'' a [] [] 0 []

-- Transform an infix expression into a prefix expression
-- interface to the actual function logic: infixToPrefix'
infixToPrefix :: [Cpt.Cpt] -> [Cpt.Cpt]
infixToPrefix a | isInfix a = trace ("infixToPrefix" ++ show a) $ insertLists $ infixToPrefix' a
                 | otherwise = a

---- helper functions

getOpPrecedence :: Cpt.Cpt -> Integer
getOpPrecedence (Cpt.Sym "^") = 3
getOpPrecedence (Cpt.Sym "/") = 2
getOpPrecedence (Cpt.Sym "*") = 2
getOpPrecedence (Cpt.Sym "+") = 1
getOpPrecedence (Cpt.Sym "-") = 1
getOpPrecedence _ = 0

reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) []

isInfix'' :: [Cpt.Cpt] -> Integer -> Bool
isInfix'' [] _ = True
isInfix'' (x : xs) i | getOpPrecedence x > 0 = isInfix' xs i
                     | otherwise = False

isInfix' :: [Cpt.Cpt] -> Integer -> Bool
isInfix' [] _ = True
isInfix' x i  | odd i = isInfix'' x $ succ i
              | otherwise = isInfix' (tail x) $ succ i

-- verifies if a given Cpt.ExprList is an infix expression
isInfix :: [Cpt.Cpt] -> Bool
isInfix a = isInfix' a 0



valid :: Cpt.Cpt -> Bool
valid (Cpt.Sym a) = False
valid _ = True

insertLists' :: [Cpt.Cpt] -> [Cpt.Cpt]
insertLists' [] = []
insertLists' (Cpt.Sym a : b : c : as)
            | valid b && valid c = [Cpt.List [Cpt.Sym a, b, c]] <> insertLists' as
            | valid b = [Cpt.List $ [Cpt.Sym a, b] <> insertLists' (c : as)]
            | otherwise = [Cpt.List $ [Cpt.Sym a] <> insertLists' (b : c : as)]
insertLists' (a : as) = a : insertLists' as


conve :: Cpt.Cpt -> [Cpt.Cpt] 
conve (Cpt.List a) = a
conve a = [a]

insertLists :: [Cpt.Cpt] -> [Cpt.Cpt]
insertLists a = trace ("insertLists" ++ show a) $ conve $ head $ insertLists' a 