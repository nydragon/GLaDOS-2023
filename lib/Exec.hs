module Exec where

import Control.Exception (throwIO)
import Exec.Builtins (execBuiltin)
import Exec.Function (lookupFunc)
import Exec.Registry (Registry, RetVal (..))
import Exec.RuntimeException
import Exec.Variables (defineVar)
import qualified Parsing.Ast as Ast

-- ─── Function Execution ──────────────────────────────────────────────────────────────────────────

regFromRet :: RetVal -> Registry
regFromRet (RetVal a b) = a

-- Bind all arguments to their values in preparation of a function call
bindArgs :: [String] -> [Ast.Expr] -> Registry -> IO Registry
bindArgs names values reg = newReg
  where
    -- Required in order to use original defineVar
    inputExprList = Ast.ExprList [Ast.ExprList [Ast.Symbole x | x <- names], Ast.ExprList values]
    newReg = regFromRet <$> defineVar [inputExprList] reg

isAtomicExpression :: Ast.Expr -> Bool
isAtomicExpression (Ast.Num n) = True
isAtomicExpression (Ast.Boolean n) = True
isAtomicExpression Ast.Null = True
isAtomicExpression _ = False

isAtomicExpressionList :: [Ast.Expr] -> Bool
isAtomicExpressionList = all isAtomicExpression

-- Executes function call
-- Note: Arguments do not need to have been reduced, execFunc takes care of it
execFunc :: String -> [Ast.Expr] -> Registry -> IO RetVal
execFunc f args _
  | not $ isAtomicExpressionList args = throwIO $ NonAtomicFunctionArgs f args
execFunc funcName argValues reg
  | Ast.isValidBuiltin funcName = execBuiltin (Ast.Call funcName argValues) reg
  | otherwise = case lookupFunc funcName (snd reg) of
      Nothing -> throwIO $ InvalidFunctionCall funcName
      Just (argNames, def) -> case def of
        Ast.Call n a -> bindArgs argNames argValues reg >>= execCall (Ast.Call n a)
        _ -> throwIO FatalError -- If body isn't an Ast.Call

-- Syntactic sugar to convert Ast.Call to args for execFunc
--
-- Will throw exception if not Ast.Call
execCall :: Ast.Expr -> Registry -> IO RetVal
execCall (Ast.Call n args) reg = execFunc n args reg
execCall _ _ = throwIO $ InvalidFunctionCall "<Unknown Function Name>"
