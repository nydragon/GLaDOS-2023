module Exec where

import Control.Exception

import qualified Parsing.Ast as Ast
import Exec.RuntimeException
import Exec.Builtins
import Exec.Eval

-- ─── Function Execution ──────────────────────────────────────────────────────────────────────────

-- Bind all arguments to their values in preparation of a function call
bindArgs :: [String] -> [Ast.Expr] -> Registry -> IO Registry
bindArgs names values reg = ret
    where
        -- Required in order to use original defineVar
        inputExprList = Ast.ExprList [(Ast.ExprList [Ast.Sym x | x <- names]), (Ast.ExprList values)]
        defineRet = defineVar inputExprList reg
        ret = fst <$> defineRet -- Gets IO a from IO (a, b)

-- Executes function call
-- Note: Arguments do not need to have been reduced, execFunc takes care of it
execFunc :: String -> [Ast.Expr] -> Registry -> IO RetVal
execFunc funcName args reg
    | isValidBuiltin funcName = execBuiltin atomicArgs
    | case lookupFunc funcName of
        Nothing -> throwIO $ InvalidFunctionCall funcName
        Just (args, def) -> case def of
            Ast.Call cName cArgs -> bindArgs cName >>= execCall (Ast.Call cName atomicCallArgs)
    -- Args reduced to atomic form
    where   atomicArgs = eval args
            atomicCallArgs = eval cArgs
            oldReg = reg    -- This won't work as it would ignore subsequent defines

-- Syntactic sugar to convert Ast.Call to args for execFunc
--
-- Will throw exception if not Ast.Call
execCall :: Ast.Expr -> Registry -> IO RetVal
execCall (Ast.Call n args) reg = execFunc n args
execCall _ _ = throwIO $ InvalidFunctionCall "<Unknown Function Name>"
