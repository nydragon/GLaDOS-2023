module Utils where

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
