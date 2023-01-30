module Parsing.Ast where

import Parsing.Cpt

-- ─── Abstract Syntaxe Tree ───────────────────────────────────────────────────────────────────────

data Expr = ExprList [Expr]
    | Num Integer
    | Symbole String
    | Call String [Expr] -- Will also be used for the boolean expression


-- This function parses lists of expressions, ignoring function calls
-- AT LEAST at first level
-- This means it will only parse function calls in sublists thanks to cptListToExpr
parseExprList :: [Cpt] -> [Expr]
parseExprList [] = []
parseExprList (x : xs) = case x of
    Sym str -> Symbole str : parseExprList xs
    Val i -> Num i : parseExprList xs
    List ls -> cptListToExpr ls : parseExprList xs

-- Parses a CPT into a single Expr value
cptListToExpr :: [Cpt] -> Expr
cptListToExpr (Sym str : xs) = if isValidBuiltin str then
    Call str (parseExprList xs) else ExprList (parseExprList xs)
cptListToExpr ls = ExprList (parseExprList ls)

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
isValidBuiltin _ = False