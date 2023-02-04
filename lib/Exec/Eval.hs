module Exec.Eval where

import qualified Parsing.Ast as Ast
import Exec.Registry
import Exec

-- ─── Evaluate Expression ─────────────────────────────────────────────────────────────────────────

-- Evaluates a given expression into an atom
eval :: Ast.Expr -> Registry -> IO RetVal
eval Ast.ExprList (x : xs) reg =
    case x of
    Ast.Call func args -> execCall (Ast.Call func (eval args))
    Ast.Symbole -> case lookupFunc

-- Dunno what either of these comments are about
-- INPUT SHOULDN'T BE LIST
-- LIST NEEDS TO BE CHECKED IF FUNCTION CALL
