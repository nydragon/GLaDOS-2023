module Parsing.Infix where

import qualified Parsing.Cpt as Cpt
import           Data.Maybe  (fromMaybe)
import           Debug.Trace (trace)

getPrecedence ::  [Cpt.Cpt] -> Integer
getPrecedence [] = 0
getPrecedence (a : _) = getOpPrecedence a

toPop :: Cpt.Cpt -> Cpt.Cpt -> Bool
toPop a b = (>) (getOpPrecedence a) (getOpPrecedence b)

process :: [Cpt.Cpt] -> [Cpt.Cpt] -> [Cpt.Cpt] -> ([Cpt.Cpt], [Cpt.Cpt])
process buffer [] _ = (buffer, [])
process buffer opStack expr 
    | (getOpPrecedence op > precedence) || null expr = process (buffer <> [op]) (init opStack) expr
    | otherwise = (buffer, opStack)
        where
            op = last opStack
            precedence =  getPrecedence expr

-- when encountering an operator we pop the operator stack onto the value stack
-- as described in Djikstra's shunting yard algorithm
popStack :: [Cpt.Cpt] -> [Cpt.Cpt] -> [Cpt.Cpt] -> Integer-> [Cpt.Cpt]
popStack [] buffer opStack i = fst $ process buffer opStack [] 
popStack (x : infixExpr) buffer opStack i =  infixToPrefix'' infixExpr newBuffer (newOpStack <> [x]) i
      where
        (newBuffer, newOpStack) = process buffer opStack (x : infixExpr)  

-- An implementation of Djikstra's shunting yard algorithm
infixToPrefix'' :: [Cpt.Cpt] -> [Cpt.Cpt] -> [Cpt.Cpt] -> Integer ->  [Cpt.Cpt]
infixToPrefix'' [x] buffer opStack i = popStack [] (buffer <> [x]) opStack i
infixToPrefix'' (x: xs) buffer opStack i
    | even i = infixToPrefix'' xs (buffer <> [x]) opStack (succ i)
    | odd i &&  (null opStack ||  (getOpPrecedence (last opStack) <= getOpPrecedence x)) = infixToPrefix'' xs buffer (opStack <> [x]) (succ i)
    | otherwise = popStack (x: xs) buffer opStack i

infixToPrefix' :: [Cpt.Cpt] -> [Cpt.Cpt]
infixToPrefix' a = reverseList $ infixToPrefix'' (reverseList a) [] [] 0

-- Transform an infix expression into a prefix expression
-- interface to the actual function logic: infixToPrefix'
infixToPrefix :: [Cpt.Cpt] -> [Cpt.Cpt]
infixToPrefix a | isInfix a = insertLists $ infixToPrefix' a
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
insertLists a = conve $ head $ insertLists' a 