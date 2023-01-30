module Parsing.Ast where

-- ─── Abstract Syntaxe Tree ───────────────────────────────────────────────────────────────────────

data Ast = Define String Ast
    | Num Int

