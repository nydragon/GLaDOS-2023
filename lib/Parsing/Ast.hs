{-# LANGUAGE InstanceSigs #-}

module Parsing.Ast where

import qualified Parsing.Cpt as Cpt
import           Data.Maybe  (fromMaybe)
import           Debug.Trace (trace)

-- ─── Abstract Syntax Tree ───────────────────────────────────────────────────────────────────────

data Expr
  = ExprList [Expr]
  | Num Integer
  | Boolean Bool
  | Symbole String
  | Call String [Expr] -- Will also be used for the boolean expression
  | Null -- Instead of using Maybe Expr
  deriving (Eq)

instance Show Expr where
  show :: Expr -> String
  show (Boolean a)
    | a = "#t"
    | otherwise = "#f"
  show (ExprList ls) = show ls
  show (Num n) = show n
  show (Symbole s) = show s
  show (Call _ _) = "#<procedure>"
  show Null = "Null"

-- ─── Parsing ─────────────────────────────────────────────────────────────────────────────────────

-- This function parses lists of expressions, ignoring function calls
-- AT LEAST at first level
-- This means it will only parse function calls in sublists thanks to parseExpr
parseExprList :: [Cpt.Cpt] -> [Expr]
parseExprList [] = []
parseExprList (Cpt.List [Cpt.Sym "define", Cpt.List (Cpt.Sym a : arg), Cpt.List body] : xs) = Call "define" [ Symbole a, Call "lambda" [parseExpr arg, parseExpr body]] : parseExprList xs
parseExprList (x : xs) = case x of
  Cpt.Sym str -> Symbole str : parseExprList xs
  Cpt.Val i -> Num i : parseExprList xs
  Cpt.List ls -> trace (show ls) $ parseExpr ls : parseExprList xs
  Cpt.Boolean b -> Boolean b : parseExprList xs

-- Parses a CPT list into a single Expr value
parseExpr :: [Cpt.Cpt] -> Expr
parseExpr (Cpt.Sym str : xs) =
  if isValidBuiltin str
    then Call str (parseExprList xs)
    else ExprList (parseExprList original)
  where
    original = Cpt.Sym str : xs
parseExpr ls = ExprList (parseExprList ls)

-- ─── Utilities ───────────────────────────────────────────────────────────────────────────────────

getOpPrecedence :: Cpt.Cpt -> Int
getOpPrecedence (Cpt.Sym "^") = 3
getOpPrecedence (Cpt.Sym "/") = 2
getOpPrecedence (Cpt.Sym "*") = 2
getOpPrecedence (Cpt.Sym "+") = 1
getOpPrecedence (Cpt.Sym "-") = 1
getOpPrecedence (Cpt.Sym _)   = 0

reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) []

popStack :: [Cpt.Cpt] -> [Cpt.Cpt] -> [Cpt.Cpt] -> Integer -> [Cpt.Cpt] -> Maybe [Cpt.Cpt]
popStack [] valStack opStack i buffer = Just (buffer ++ valStack ++ opStack)
popStack infixExpr valStack opStack i buffer
    = trace ("popstack : " ++ show buffer) $ infixToPostfix' infixExpr [] [] i (buffer ++ valStack ++ opStack)

infixToPostfix' :: [Cpt.Cpt] -> [Cpt.Cpt] -> [Cpt.Cpt] -> Integer -> [Cpt.Cpt] ->  Maybe [Cpt.Cpt]
infixToPostfix' [x] valStack opStack i buffer = popStack [] ([x] <> valStack) opStack i buffer
infixToPostfix' (x: xs) valStack opStack i buffer
    | i == 0 || even i = infixToPostfix' xs ([x] <> valStack) opStack (succ i) buffer
    | not $ even i  && getOpPrecedence x <= getOpPrecedence (last opStack)
        = infixToPostfix' xs valStack (opStack <> [x]) (succ i) buffer
    | otherwise = popStack (x : xs) valStack opStack i buffer
infixToPostfix'  x valStack opStack i buffer = Just buffer

infixToPostfix :: [Cpt.Cpt] -> [Cpt.Cpt]
infixToPostfix a = reverseList $ fromMaybe [] $ infixToPostfix' a [] [] 0 []

-- Utility function for execution
-- Converts cpt list to Expr Call
-- IMPORTANT : Returns nothing in case of error
exprListToCall :: [Expr] -> Maybe Expr
exprListToCall [] = Nothing
exprListToCall (Symbole name : xs) = Just (Call name xs)
exprListToCall _ = Nothing

-- This is where we put builtins
isValidBuiltin :: String -> Bool
isValidBuiltin "define" = True
isValidBuiltin "lambda" = True
isValidBuiltin "+" = True
isValidBuiltin "-" = True
isValidBuiltin "div" = True
isValidBuiltin "*" = True
isValidBuiltin "mod" = True
isValidBuiltin "<" = True
isValidBuiltin "<=" = True
isValidBuiltin ">" = True
isValidBuiltin ">=" = True
isValidBuiltin "if" = True
isValidBuiltin "println" = True
isValidBuiltin "print" = True
isValidBuiltin "eq?" = True
isValidBuiltin "noop" = True -- Should be useful in the future, will return list of args
isValidBuiltin _ = False

-- Returns boolean if Expr is atomic. This means it cannot be further reduced.
-- Note: Sym is not atomic as it needs to be reduced to a value
isAtomic :: Expr -> Bool
isAtomic (Num _) = True
isAtomic (Boolean _) = True
isAtomic Null = True
isAtomic _ = False

-- Checks if list is atomic
isListAtomic :: [Expr] -> Bool
isListAtomic = foldr ((&&) . isAtomic) True