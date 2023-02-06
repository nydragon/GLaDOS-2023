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
eval (Ast.ExprList (Ast.ExprList [Ast.Symbole "lambda", Ast.ExprList args, Ast.ExprList body] : argValues)) reg = evalLambda args body argValues reg
eval (Ast.ExprList ls) reg = evalExprList ls reg
eval (Ast.Call "define" args) reg = execCall (Ast.Call "define" args) reg
eval (Ast.Call fn args) reg = do
    -- Pattern match return of eval of args
    RetVal a b <- eval (Ast.ExprList args) reg

    case b of
        Ast.ExprList c -> execCall (Ast.Call fn c) a
        _ -> throwIO FatalError
eval (Ast.Symbole s) (v, f) = case lookupVar s v of
  Nothing -> return $ RetVal (v, f) (Ast.Symbole s)
  Just x -> return $ RetVal (v, f) x
eval x reg = return $ RetVal reg x

-- Dunno what either of these comments are about
-- INPUT SHOULDN'T BE LIST
-- LIST NEEDS TO BE CHECKED IF FUNCTION CALL

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
      Just (Ast.ExprList [Ast.Symbole "lambda", Ast.ExprList args, Ast.ExprList body]) -> evalLambda  args body argValues reg

makeLambda :: [Ast.Expr] -> [Ast.Expr] -> [Ast.Expr] -> Ast.Expr
makeLambda args body argValues = Ast.ExprList (Ast.ExprList [Ast.Symbole "lambda", Ast.ExprList args, Ast.ExprList body] : argValues)

evalLambda :: [Ast.Expr] -> [Ast.Expr] -> [Ast.Expr] -> Registry -> IO RetVal
evalLambda args body argValues = evalLambda' (makeLambda args body argValues)

evalLambda' :: Ast.Expr -> Registry -> IO RetVal
evalLambda' (Ast.ExprList (Ast.ExprList [Ast.Symbole "lambda", Ast.ExprList args, Ast.ExprList body] : argValues)) reg = do
    tempReg <- bindArgs (toString args) argValues reg
    (RetVal a v) <- eval (Ast.ExprList body) tempReg
    return (RetVal reg v)
evalLambda' a _ = throwIO $ InvalidFunctionCall "lambda"

-- Ast.Call n a -> bindArgs argNames argValues reg >>= execCall (Ast.Call n a) >>= unbindArgs argNames
-- _ -> throwIO FatalError -- If body isn't an Ast.Call

-- Syntactic sugar to convert Ast.Call to args for execFunc
--
-- Will throw exception if not Ast.Call
execCall :: Ast.Expr -> Registry -> IO RetVal
execCall (Ast.Call n args) reg =  execFunc n args reg
execCall _ _ = throwIO $ InvalidFunctionCall "<Unknown Function Name>"
