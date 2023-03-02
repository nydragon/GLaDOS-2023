module Exec.Utils where

import qualified Parsing.Ast as Ast
import Exec.Registry (Registry, RetVal (..))
import Debug.Trace

convert :: Ast.Expr -> [Ast.Expr]
convert (Ast.ExprList a) = a
convert a = [a]

unpack :: RetVal -> (Ast.Expr, Registry)
unpack (RetVal reg expr) = (expr, reg)

-- Returns boolean if Expr is atomic. This means it cannot be further reduced.
-- Note: Sym is not atomic as it needs to be reduced to a value
isAtomic :: Ast.Expr -> Bool
isAtomic (Ast.Num n) = True
isAtomic (Ast.Flt n) = True
isAtomic (Ast.Boolean n) = True
isAtomic (Ast.Literal n) = True
isAtomic (Ast.Handle n) = True
isAtomic Ast.Null = True
isAtomic (Ast.Symbole _) = True
isAtomic (Ast.ExprList list) = isAtomicList list
isAtomic a = False

-- Checks if list is atomic
isAtomicList :: [Ast.Expr] -> Bool
isAtomicList = all isAtomic