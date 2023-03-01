module Exec.Utils where

import qualified Parsing.Ast as Ast
import Exec.Registry (Registry, RetVal (..))

convert :: Ast.Expr -> [Ast.Expr]
convert (Ast.ExprList a) = a
convert a = [a]

unpack :: RetVal -> (Ast.Expr, Registry)
unpack (RetVal reg expr) = (expr, reg)
