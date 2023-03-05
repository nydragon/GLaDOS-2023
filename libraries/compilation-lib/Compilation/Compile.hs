module Compilation.Compile where

import Control.Exception
import Data.Maybe
import Compilation.Function
import Compilation.Utils
import qualified Parsing.Ast as Ast
import Debug.Trace

-- ─── Evaluate Expression ─────────────────────────────────────────────────────────────────────────

-- Special eval function for Ast.List
evalExprList' :: [Ast.Expr] -> Registry -> IO RetVal
evalExprList' [] reg = return $ RetVal reg (Ast.ExprList [])
evalExprList' (x : xs) reg = do
    RetVal evalReg evalOutput <- eval x reg

    (RetVal recursiveReg exprList) <- evalExprList' xs evalReg

    return (RetVal recursiveReg (Ast.ExprList $ evalOutput : convert exprList))

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
            evalExprList' (Ast.Symbole s : xs) reg -- If not valid function call
    where
        exprList = Ast.Symbole s : xs
        reg = (v, f)
evalExprList ls reg = evalExprList' ls reg

-- Evaluates a given expression into an atom
-- Note : Redirects evalutation to evalExprList if Ast.Expr
eval :: Ast.Expr -> Registry -> IO RetVal
-- Lambda called on definition
eval (Ast.ExprList (Ast.Call "lambda" [Ast.ExprList args, body] : argValues)) reg = evalLambda args body argValues reg
-- Basic ExprList
eval (Ast.ExprList ls) reg = evalExprList ls reg
-- Function definition
eval (Ast.Call "define" (sym : Ast.Call "lambda" args : r)) reg = execCall (Ast.Call "define" (sym : Ast.Call "lambda" args : r)) reg
-- Conditional
eval (Ast.Call "if" (x : xs)) reg = do
    -- Evaluate first argument
    RetVal a condVal <- eval x reg
    let newArgs = condVal : xs

    -- Run if
    RetVal b outputExpr <- execBuiltin (Ast.Call "if" newArgs) a

    -- Eval and return outputExpr
    eval outputExpr b
-- Function call
eval (Ast.Call fn args) reg = do
    -- Pattern match return of evaluation of args
    RetVal a b <- eval (Ast.ExprList args) reg

    -- Extract arg list from return and call function
    case b of
        Ast.ExprList c -> execCall (Ast.Call fn c) a
        _ -> throwIO FatalError
-- This pattern match should probably throw an error
eval x reg = return $ RetVal reg x

-- Executes function call
-- Note: Arguments do not need to have been reduced, execFunc takes care of it
-- execFunc :: String -> [Ast.Expr] -> Registry -> IO RetVal
-- execFunc "define" args reg = execBuiltin (Ast.Call "define" args) reg
-- execFunc f args _
--     | not $ isAtomicList args = throwIO $ NonAtomicFunctionArgs f args
-- execFunc funcName argValues reg
--     | Ast.isValidBuiltin funcName = execBuiltin (Ast.Call funcName argValues) reg
--     | otherwise = case lookupFunc funcName (snd reg) of
--         Nothing -> throwIO $ InvalidFunctionCall funcName
--         Just (Ast.ExprList [Ast.ExprList args, body]) -> evalLambda args body argValues reg
--         _ -> throwIO FatalError

-- Syntactic sugar to convert Ast.Call to args for execFunc
--
-- Will throw exception if not Ast.Call
execCall :: Ast.Expr -> Registry -> IO RetVal
execCall (Ast.Call n args) reg =  execFunc n args reg
execCall _ _ = throwIO $ InvalidFunctionCall "<Unknown Function Name>"

