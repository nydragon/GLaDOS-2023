module Exec.Builtins where

import Exec.Lookup
import qualified Parsing.Ast as Ast
import Control.Exception (throwIO)
import Exec.RuntimeException

-- ─── Builtin Execution ───────────────────────────────────────────────────────────────────────────

-- Executes an Expr.Call that has been confirmed to be a builtin function
--
-- args : Expr.Call -> Lookup
execBuiltin :: Ast.Expr -> Lookup -> IO Lookup
execBuiltin (Ast.Call func ls) reg = case func of
    "println" -> printBuiltin ls reg
    _ -> return reg
execBuiltin _ _ = throwIO UndefinedBehaviour -- Builtin not found

-- ─── Builtin Implementations ─────────────────────────────────────────────────────────────────────

printBuiltin :: [Ast.Expr] -> Lookup -> IO Lookup
printBuiltin ls reg = print (head ls) >> return reg

division :: Integer -> Integer -> Either Integer String
division a 0 = Right "ZeroDivisionError"
division a b = Left (div a b)

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