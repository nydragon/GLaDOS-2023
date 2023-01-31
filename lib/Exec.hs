module Exec where

import qualified Parsing.Ast as Ast
import Exec.Lookup

-- ─── Main Function ───────────────────────────────────────────────────────────────────────────────

-- Runs a given list of expressions
--
-- Expects Expr to be a valid call
-- run :: [Ast.Expr] ->