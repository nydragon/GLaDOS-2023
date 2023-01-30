module Parsing.Ast where

-- ─── Abstract Syntaxe Tree ───────────────────────────────────────────────────────────────────────

data Expr = List [Expr]
    | Num Integer
    | Sym String
    | Call String [Expr] -- Will also be used for the boolean expression

-- DEFINITIONS
    -- Expression = Anything that ends up a value
    -- Function Call = When first element of list is symbol

-- NOTES
    -- DID NOT implement ternary (if else) statement for the time being