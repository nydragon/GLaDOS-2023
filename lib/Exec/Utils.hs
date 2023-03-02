module Exec.Utils where

import qualified Parsing.Ast as Ast
import Exec.Registry (Registry, RetVal (..))
import Debug.Trace
import Data.Char (isDigit)

convert :: Ast.Expr -> [Ast.Expr]
convert (Ast.ExprList a) = a
convert a = [a]

unpack :: RetVal -> (Ast.Expr, Registry)
unpack (RetVal reg expr) = (expr, reg)


isNumeric :: String -> Bool
isNumeric str | all isDigit str = True
isNumeric str | not (null (tail str)) && all isDigit (tail str) && head str == '-' = True
isNumeric str | isFloat str = True
isNumeric str | not (null (tail str)) && isFloat (tail str) && head str == '-' = True
isNumeric _ = False

parseNum :: String -> Ast.Expr
parseNum str | all isDigit str = Ast.Num (read str :: Integer)
parseNum str | not (null (tail str)) && all isDigit (tail str) && head str == '-' =  Ast.Num $ negate (read (tail str) :: Integer)
parseNum str | isFloat str = Ast.Flt (read str :: Float)
parseNum str | not (null (tail str)) && isFloat (tail str) && head str == '-' = Ast.Flt $ negate (read (tail str) :: Float)

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

isFloat' :: String -> Integer -> Bool -> Bool
isFloat' [x] c True | isDigit x = True
isFloat' (x:xs) c point | isDigit x = isFloat' xs (succ c) point
isFloat' ('.':xs) c False = isFloat' xs 0 True
isFloat' [x] c False | isDigit x = False
isFloat' _ _ _ = False

isFloat :: String -> Bool
isFloat s = isFloat' s 0 False