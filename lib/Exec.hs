module Exec where

import Control.Exception

import qualified Parsing.Ast as Ast
import Exec.Lookup
import Exec.RuntimeException

-- ─── Main Function ───────────────────────────────────────────────────────────────────────────────

-- Runs a given list of expressions
--
-- Expects all base expressions to be valid function calls
-- runExprList' :: [Ast.Expr] -> Lookup -> IO Lookup
-- runExprList' 

-- Entry point function
runExprList :: [Ast.Expr] -> IO ()
runExprList _ = throwIO (InvalidFunctionCall "funcName")