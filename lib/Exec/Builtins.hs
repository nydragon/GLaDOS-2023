module Exec.Builtins where

import Control.Exception (throwIO)
import Data.Typeable

import qualified Parsing.Ast as Ast
import Exec.RuntimeException
import Exec.Registry

-- Function declarations should use the same prototype :
-- [Ast.Expr] -> Registry -> IO RetVal
--
-- The first list is a list of all arguments
-- Registry is the registry
--
-- Returns RetVal

-- ─── Builtin Execution ───────────────────────────────────────────────────────────────────────────

-- Executes an Expr.Call that has been confirmed to be a builtin function
--
-- args : Expr.Call -> Registry
execBuiltin :: Ast.Expr -> Registry -> IO RetVal
execBuiltin (Ast.Call func ls) reg = case func of
    "println" -> printBuiltin ls reg
    "/" -> divBuiltin ls reg
    _ -> throwIO NotYetImplemented
execBuiltin _ _ = throwIO UndefinedBehaviour -- Builtin not found

-- ─── Builtin Implementations ─────────────────────────────────────────────────────────────────────

printBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
printBuiltin ls reg = print (head ls) >> return output
    where   output = RetVal reg Ast.Null

divBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
divBuiltin (Ast.Num a : Ast.Num b : _) reg = if b == 0 then throwIO NullDivision else return output
    where   output = RetVal reg $ Ast.Num (div a b)
divBuiltin (Ast.Num a : x : xs) _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName x)

modulo :: Integer -> Integer -> Either Integer String
modulo a 0 = Right "ZeroDivisionError"
modulo a b = Left (mod a b)

multiply :: Integer -> Integer -> Integer
multiply a b = a * b

substract :: Integer -> Integer -> Integer
substract a b = a - b

add :: Integer -> Integer ->   Integer

add a b = a + b

lt :: (Ord a) => a -> a -> Bool
lt a b = a < b

-- ─── Utilities ───────────────────────────────────────────────────────────────────────────────────

-- Get string representation of type name
-- This will most likely have to be moved
getTypeName :: Typeable a => a -> String
getTypeName = show . typeOf