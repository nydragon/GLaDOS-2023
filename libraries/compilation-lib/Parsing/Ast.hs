{-# LANGUAGE InstanceSigs #-}

module Parsing.Ast where

import qualified Parsing.Cpt as Cpt
import Parsing.Infix (infixToPrefix)
import GHC.IO.Handle

-- ─── Abstract Syntax Tree ───────────────────────────────────────────────────────────────────────

data Expr
  = ExprList [Expr]
  | Num Integer
  | Flt Float
  | Boolean Bool
  | Symbole String
  | Literal String
  | Call String [Expr] -- Will also be used for the boolean expression
  | Handle Handle -- A file handle
  | Null -- Instead of using Maybe Expr
  deriving (Eq)

instance Show Expr where
  show :: Expr -> String
  show (Boolean a)
    | a = "#t"
    | otherwise = "#f"
  show (ExprList ls) = show ls
  show (Num n) = show n
  show (Flt n) = show n
  show (Symbole s) = show s
  show (Literal s) = show s
  show (Handle s) = show s
  show (Call _ _) = "#<procedure>"
  show Null = "NULL"

-- ─── Parsing ─────────────────────────────────────────────────────────────────────────────────────

-- This function parses lists of expressions, ignoring function calls
-- AT LEAST at first level
-- This means it will only parse function calls in sublists thanks to parseExpr
parseExprList :: [Cpt.Cpt] -> [Expr]
parseExprList [] = []
parseExprList (Cpt.List [Cpt.Sym "define", Cpt.List (Cpt.Sym a : arg), Cpt.List body] : xs) = Call "define" [ Symbole a, Call "lambda" [parseExpr arg, parseExpr $ infixToPrefix body]] : parseExprList xs
parseExprList (x : xs) = case x of
  Cpt.Sym str -> Symbole str : parseExprList xs
  Cpt.Val i -> Num i : parseExprList xs
  Cpt.Floating i -> Flt i : parseExprList xs
  Cpt.List ls -> parseExpr (infixToPrefix ls) : parseExprList xs
  Cpt.Boolean b -> Boolean b : parseExprList xs
  Cpt.Literal' b -> Literal b : parseExprList xs

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
isValidBuiltin "/" = True
isValidBuiltin "*" = True
isValidBuiltin "%" = True
isValidBuiltin "<" = True
isValidBuiltin "<=" = True
isValidBuiltin ">" = True
isValidBuiltin ">=" = True
isValidBuiltin "if" = True
isValidBuiltin "println" = True
isValidBuiltin "print" = True
isValidBuiltin "eq?" = True
isValidBuiltin "readFile" = True
isValidBuiltin "openFile" = True
isValidBuiltin "head" = True
isValidBuiltin "tail" = True
isValidBuiltin "init" = True
isValidBuiltin "last" = True
isValidBuiltin "join" = True
isValidBuiltin "read" = True
isValidBuiltin "readInt" = True
isValidBuiltin _ = False

