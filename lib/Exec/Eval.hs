module Exec.Eval where

import Control.Exception
import Data.Maybe
import Exec
import Exec.Function
import Exec.Registry
import Exec.RuntimeException
import Exec.Variables
import qualified Parsing.Ast as Ast
import Debug.Trace
import Exec.Builtins
import qualified Parsing.Ast as Eval

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
    Just a -> case Ast.exprListToCall exprList of
        Nothing -> throwIO FatalError
        Just call -> eval call reg
    Nothing -> if Ast.isValidBuiltin s then
        case Ast.exprListToCall exprList of
            Nothing -> throwIO FatalError
            Just call -> eval call reg
        else
            utilConvert $ evalExprList' (Ast.Symbole s : xs) reg -- If not valid function call
    where
        exprList = Ast.Symbole s : xs
        reg = (v, f)
evalExprList ls reg = utilConvert $ evalExprList' ls reg

-- Evaluates a given expression into an atom
-- Note : Redirects evalutation to evalExprList if Ast.Expr
eval :: Ast.Expr -> Registry -> IO RetVal
eval (Ast.ExprList (Ast.Call "lambda" [Ast.ExprList args, body] : argValues)) reg = evalLambda args body argValues reg
eval (Ast.ExprList ls) reg = evalExprList ls reg
-- Function special cases
eval (Ast.Call "define" args) reg = execCall (Ast.Call "define" args) reg
eval (Ast.Call "if" (x : xs)) reg = do
    -- Evaluate first argument
    RetVal a condVal <- eval x reg
    let newArgs = condVal : xs

    -- Run if
    RetVal b outputExpr <- execBuiltin (Ast.Call "if" newArgs) a

    -- Eval and return outputExpr
    eval outputExpr b
eval (Ast.Call fn args) reg = do
    -- Pattern match return of evaluation of args
    RetVal a b <- eval (Ast.ExprList args) reg

    -- Extract arg list from return and call function
    case b of
        Ast.ExprList c -> execCall (Ast.Call fn c) a
        _ -> throwIO FatalError
eval (Ast.Symbole s) (v, f) = case lookupVar s v of
  Nothing -> return $ RetVal (v, f) (Ast.Symbole s)
  Just x -> return $ RetVal (v, f) x
eval x reg = return $ RetVal reg x

-- transforms a list of Symboles into a list of Strings
toString :: [Ast.Expr] -> [String]
toString [Ast.Symbole x] = [x]
toString (Ast.Symbole x: xs) = x : toString xs
toString _  = []

-- Executes function call
-- Note: Arguments do not need to have been reduced, execFunc takes care of it
execFunc :: String -> [Ast.Expr] -> Registry -> IO RetVal
execFunc "define" args reg = execBuiltin (Ast.Call "define" args) reg
execFunc f args _
  | not $ isAtomicExpressionList args = throwIO $ NonAtomicFunctionArgs f args
execFunc funcName argValues reg
  | Ast.isValidBuiltin funcName = execBuiltin (Ast.Call funcName argValues) reg
  | otherwise = case lookupFunc funcName (snd reg) of
      Nothing -> throwIO $ InvalidFunctionCall funcName
      Just (Ast.ExprList [Ast.ExprList args, body]) -> evalLambda args body argValues reg
      _ -> throwIO FatalError

-- Evaluates a lambda expression
-- Accepts: Expression describing the arguments [Symbole "a", Symbole "b"],
--          Expression describing the behaviour Call "+" [Symbole "a", Symbole "b"]],
--          Expression describing the arguments' values [Num 2, Num 3]

-- Argument values are bound to arguments based on order
evalLambda :: [Ast.Expr] -> Ast.Expr -> [Ast.Expr] -> Registry -> IO RetVal
evalLambda args body val reg = do
    tempReg <- bindArgs (toString args) val reg
    (RetVal a v) <- eval body tempReg
    return (RetVal reg v)

-- Syntactic sugar to convert Ast.Call to args for execFunc
--
-- Will throw exception if not Ast.Call
execCall :: Ast.Expr -> Registry -> IO RetVal
execCall (Ast.Call n args) reg =  execFunc n args reg
execCall _ _ = throwIO $ InvalidFunctionCall "<Unknown Function Name>"
