module Utils where

import Data.Char (isDigit)

isValidBuiltin :: String -> Bool
isValidBuiltin "define" = True
isValidBuiltin "lambda" = True
isValidBuiltin "+" = True
isValidBuiltin "-" = True
isValidBuiltin "/" = True
isValidBuiltin "*" = True
isValidBuiltin "%" = True
isValidBuiltin "<" = True
isValidBuiltin "<=" = True
isValidBuiltin ">" = True
isValidBuiltin ">=" = True
isValidBuiltin "if" = True
isValidBuiltin "println" = True
isValidBuiltin "print" = True
isValidBuiltin "==" = True
isValidBuiltin "/=" = True
isValidBuiltin "readFile" = True
isValidBuiltin "openFile" = True
isValidBuiltin "head" = True
isValidBuiltin "tail" = True
isValidBuiltin "init" = True
isValidBuiltin "last" = True
isValidBuiltin "join" = True
isValidBuiltin "read" = True
isValidBuiltin "readInt" = True
isValidBuiltin _ = False

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