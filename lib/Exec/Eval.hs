module Exec.Eval where

import Control.Exception
import Data.Maybe
import Exec
import Exec.Function
import Exec.Registry
import Exec.RuntimeException
import Exec.Variables
import qualified Parsing.Ast as Ast

-- ─── Evaluate Expression ─────────────────────────────────────────────────────────────────────────

-- Utility function for converting IO (Registry, [Ast.Expr])
-- to IO RetVal (reg, Ast.ExprList)

utilConvert :: IO (Registry, [Ast.Expr]) -> IO RetVal
utilConvert og = do
  (reg, ls) <- og

  return $ RetVal reg (Ast.ExprList ls)

-- Special eval function for Ast.List
evalExprList' :: [Ast.Expr] -> Registry -> IO (Registry, [Ast.Expr])
evalExprList' [] reg = return (reg, [])
evalExprList' (x : xs) reg = do
  RetVal evalReg evalOutput <- eval x reg

  (recursiveReg, exprList) <- evalExprList' xs evalReg

  return (recursiveReg, evalOutput : exprList)

-- This function first checks if expression list is valid
evalExprList :: [Ast.Expr] -> Registry -> IO RetVal
evalExprList (Ast.Symbole s : xs) (v, f) = case lookupFunc s f of
  Nothing -> utilConvert $ evalExprList' (Ast.Symbole s : xs) reg -- If not valid function call
  Just _ -> case Ast.exprListToCall exprList of
    Nothing -> throwIO FatalError
    Just call -> eval call reg
  where
    exprList = Ast.Symbole s : xs
    reg = (v, f)
evalExprList ls reg = utilConvert $ evalExprList' ls reg

-- Evaluates a given expression into an atom
-- Note : Redirects evalutation to evalExprList if Ast.Expr
eval :: Ast.Expr -> Registry -> IO RetVal
eval (Ast.ExprList ls) reg = evalExprList ls reg
eval (Ast.Call fn args) reg = do
    -- Pattern match return of eval of args
    RetVal a b <- eval (Ast.ExprList args) reg

    case b of
        Ast.ExprList c -> execCall (Ast.Call fn c) a
        _ -> throwIO FatalError

eval (Ast.Symbole s) (v, f) = case lookupVar s v of
  Nothing -> return $ RetVal (v, f) (Ast.Symbole s)
  Just x -> return $ RetVal (v, f) x

-- Dunno what either of these comments are about
-- INPUT SHOULDN'T BE LIST
-- LIST NEEDS TO BE CHECKED IF FUNCTION CALL
