module Exec.Function where

import qualified Data.Map as Map
import Control.Exception

import qualified Parsing.Ast as Ast
import Exec.Registry
import Exec.RuntimeException

-- IMPORTANT : This file must NOT be imported in Exec module. (Cyclic Dependencies)

-- ─── Definition And Getting ──────────────────────────────────────────────────────────────────────

-- THIS IS TO BE CALLED FROM THE BUILTINS MODULE

-- Defines a function
-- Returns Nothing if name is alreadt used
--
-- Args : define args -> registry
-- Note : Returns retval as define is a function (returns NULL)
-- EXPECTS : Args to contain two sublists (hence big pattern match)
defineFunc :: [Ast.Expr] -> Registry -> IO RetVal
defineFunc (Ast.ExprList (Ast.Symbole n : args) : Ast.ExprList def : _) (v, f) =
    if isArgumentList args then
        case Ast.exprListToCall def of
            Nothing -> throwIO $ InvalidFunctionDefinition n
            -- Updated function registry and return Null
            Just x -> return $ RetVal (v, Map.insert n (argNameStr, x) f) Ast.Null
    else throwIO $ InvalidFunctionDefinition n
        where   argNameStr = [str | Ast.Symbole str <- args]
definefunc _ _ = throwIO $ InvalidFunctionDefinition "<Unknown Function Name>"

-- Get definitions
-- Returns an ExprList representing body of function
lookupFunc :: String -> FuncRegistry -> Maybe Function
lookupFunc = Map.lookup

-- ─── Utilities ───────────────────────────────────────────────────────────────────────────────────

-- Checks if argument list is valid (only symboles)
isArgumentList :: [Ast.Expr] -> Bool
isArgumentList [] = True
isArgumentList (Ast.Symbole _ : xs) = isArgumentList xs
isArgumentList _ = False
