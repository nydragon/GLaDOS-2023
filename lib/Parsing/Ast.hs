module Parsing.Ast where

import Parsing.Cpt

-- ─── Abstract Syntaxe Tree ───────────────────────────────────────────────────────────────────────

data Expr = ExprList [Expr]
    | Num Integer
    | Symbole String
    | Call String [Expr] -- Will also be used for the boolean expression

-- This function parses lists of expressions, ignoring function calls
-- AT LEAST at first level
-- This means it will only parse function calls in sublists thanks to parseExpr
parseExprList :: [Cpt] -> [Expr]
parseExprList [] = []
parseExprList (x : xs) = case x of
    Sym str -> Symbole str : parseExprList xs
    Val i -> Num i : parseExprList xs
    List ls -> parseExpr ls : parseExprList xs

-- Parses a CPT into a single Expr value
parseExpr :: [Cpt] -> Expr
parseExpr (Sym str : xs) = if isValidBuiltin str then
    Call str (parseExprList xs) else ExprList (parseExprList xs)
parseExpr ls = ExprList (parseExprList ls)

-- Utility function for execution
-- Converts cpt list to Expr Call
-- IMPORTANT : Returns nothing in case of error
exprListToCall :: [Expr] -> Maybe Expr
exprListToCall [] = Nothing
exprListToCall (Symbole name : xs) = Just (Call name xs)
exprListToCall _ = Nothing

-- DEFINITIONS
    -- Expression = Anything that ends up a value
    -- Function Call = When first element of list is symbol

-- NOTES
    -- DID NOT implement ternary (if else) statement for the time being
    -- At execution : if list is deemed to be a function call, create function to convert it

-- This is where we put builtins
isValidBuiltin :: String -> Bool
isValidBuiltin "define" = True
isValidBuiltin "lambda" = True
isValidBuiltin "add" = True
isValidBuiltin "sub" = True
isValidBuiltin "div" = True
isValidBuiltin "mul" = True
isValidBuiltin "noop" = True
isValidBuiltin _ = False