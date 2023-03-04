module Compilation.Utils where

import qualified Parsing.Ast as Ast
import Debug.Trace
import Data.Char (isDigit)

convert :: Ast.Expr -> [Ast.Expr]
convert (Ast.ExprList a) = a
convert a = [a]

isPositiveInt :: String -> Bool
isPositiveInt str | all isDigit str = True
isPositiveInt _ = False

isNegativeInt :: String -> Bool
isNegativeInt str | not (null (tail str)) && all isDigit (tail str) && head str == '-' = True
isNegativeInt _ = False

isPositiveFloat' :: String -> Integer -> Bool -> Bool
isPositiveFloat' [x] c True | isDigit x = True
isPositiveFloat' (x:xs) c point | isDigit x = isPositiveFloat' xs (succ c) point
isPositiveFloat' ('.':xs) c False = isPositiveFloat' xs 0 True
isPositiveFloat' [x] c False | isDigit x = False
isPositiveFloat' _ _ _ = False

isPositiveFloat :: String -> Bool
isPositiveFloat s = isPositiveFloat' s 0 False

isNegativeFloat :: String -> Bool
isNegativeFloat str | not (null (tail str)) && isPositiveFloat (tail str) && head str == '-' = True
isNegativeFloat _ = False

isNumeric :: String -> Bool
isNumeric str | isPositiveInt str = True
isNumeric str | isNegativeInt str = True
isNumeric str | isPositiveFloat str = True
isNumeric str | isNegativeFloat str = True
isNumeric _ = False

parseNum :: String -> Ast.Expr
parseNum str | isPositiveInt str = Ast.Num (read str :: Integer)
parseNum str | isNegativeInt str =  Ast.Num $ negate (read (tail str) :: Integer)
parseNum str | isPositiveFloat str = Ast.Flt (read str :: Float)
parseNum str | isNegativeFloat str = Ast.Flt $ negate (read (tail str) :: Float)

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