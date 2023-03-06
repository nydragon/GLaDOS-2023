module Exec.Utils where

import Exec.InferType (Type, Registers)
import qualified Exec.InferType as Type
import Data.Char (isDigit)
import Data.Map (insert, lookup)

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

parseNum :: String -> Type
parseNum str | isPositiveInt str = Type.Integer (read str :: Integer)
parseNum str | isNegativeInt str =  Type.Integer $ negate (read (tail str) :: Integer)
parseNum str | isPositiveFloat str = Type.Float (read str :: Float)
parseNum str | isNegativeFloat str = Type.Float $ negate (read (tail str) :: Float)

-- Assign value Type to #RET
assignRet :: Type -> Registers -> Registers
assignRet = insert (Type.Symbol "#RET")

lookupRet :: Registers -> Maybe Type
lookupRet = Data.Map.lookup (Type.Symbol "#RET")