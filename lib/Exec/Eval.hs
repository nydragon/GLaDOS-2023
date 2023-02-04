module Exec.Eval where

import qualified Parsing.Ast as Ast
import Exec.Registry

-- ─── Evaluate Expression ─────────────────────────────────────────────────────────────────────────

-- Evaluates a given expression into an atom
eval :: [Ast.Expr] -> Registry -> IO RetVal
eval (Ast.Call func ls : xs) reg = -- INPUT SHOULDN'T BE LIST
-- LIST NEEDS TO BE CHECKED IF FUNCTION CALL
