module Exec.Eval where

import qualified Parsing.Ast as Ast
import Exec.Registry
import Exec.Function
import Exec

-- ─── Evaluate Expression ─────────────────────────────────────────────────────────────────────────

-- Special eval function for Ast.List
evalExprList' :: [Ast.Expr] -> Registry -> (reg, [Ast.Expr])
evalExprList' [] reg = (reg, ExprList [])
evalExprList' (x : xs) reg = ()
    where
        (evalReg, evalOutput) = eval x reg
        (recursiveReg, exprList) = evalExprList' xs evalReg

evalExprList :: [Ast.Expr] -> Registry -> IO RetVal
evalExprList (Ast.Symbole s : xs) reg = case lookupFunc s of
    Nothing -> evalExprList (Ast.Symbole s : xs)
    Just (x) -> execFuncCall x reg
evalExprList ls reg = evalExprList' ls reg

-- Evaluates a given expression into an atom
-- Note : Redirects evalutation to evalExprList if Ast.Expr
eval :: Ast.Expr -> Registry -> IO RetVal
eval (Ast.ExprList ls) reg = 

-- Dunno what either of these comments are about
-- INPUT SHOULDN'T BE LIST
-- LIST NEEDS TO BE CHECKED IF FUNCTION CALL
