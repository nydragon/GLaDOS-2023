module Compilation.Utils where

import qualified Parsing.Ast as Ast
import Utils (isPositiveInt, isNegativeInt, isPositiveFloat, isNegativeFloat)

convert :: Ast.Expr -> [Ast.Expr]
convert (Ast.ExprList a) = a
convert a = [a]

parseNum :: String -> Ast.Expr
parseNum str | isPositiveInt str = Ast.Num (read str :: Integer)
parseNum str | isNegativeInt str =  Ast.Num $ negate (read (tail str) :: Integer)
parseNum str | isPositiveFloat str = Ast.Flt (read str :: Float)
parseNum str | isNegativeFloat str = Ast.Flt $ negate (read (tail str) :: Float)

isSymbol :: Ast.Expr -> Bool
isSymbol (Ast.Symbole _) = True
isSymbol _ = False

-- Returns boolean if Expr is atomic. This means it cannot be further reduced.
-- Note: Sym is not atomic as it needs to be reduced to a value
isAtomic :: Ast.Expr -> Bool
isAtomic (Ast.ExprList list) = isAtomicList list
isAtomic (Ast.Symbole _) = False
isAtomic (Ast.Call _ _) = False
isAtomic _ = True

-- Checks if list is atomic
isAtomicList :: [Ast.Expr] -> Bool
isAtomicList = all isAtomic