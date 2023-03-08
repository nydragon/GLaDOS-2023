module Exec.Infer where
import qualified Exec.InferType as Type
import Exec.InferType (Type)
import Exec.Utils (parseNum)
import Data.Char (isAlpha)
import Utils (isValidBuiltin, isNumeric)
import Control.Exception (throw)
import Exec.RuntimeException (RuntimeException(UnknownType))

isString :: String -> Bool
isString str | head str == '"' && last str == '"' = True
isString _ = False

isSymbol :: String -> Bool
isSymbol str | all isAlpha str || isValidBuiltin str || str == "#RET" = True
isSymbol _ = False

isList :: String -> Bool
isList ls | head ls == '[' && last ls == ']' = True
isList _ = False

getNextElem' :: String -> String -> Integer -> (String, String)
getNextElem' buff (',':rest) 0 = (buff, ',': rest)
getNextElem' buff (']': rest) openB
    | openB == 1 = (buff ++ "]", rest)
    | openB == 0 = (buff, rest)
    | otherwise = getNextElem' (buff ++ [']']) rest (pred openB)
getNextElem' buff ('[': rest) openB = getNextElem' (buff ++ ['[']) rest (succ openB)
getNextElem' buff (c:cs) openB = getNextElem' (buff ++ [c]) cs openB
getNextElem' buff [] _ = (buff,"")

getNextElem :: String -> (String, String)
getNextElem str = getNextElem' "" str 0

parseList :: String -> [Type]
parseList "" = []
parseList str
    | not $ null e = infer e : parseList rest
    | otherwise = []
    where (e, rest) = getNextElem $ tail str

removeEscapes :: String -> String
removeEscapes ('\\':xs)  = removeEscapes xs
removeEscapes (x:xs)  = x : removeEscapes xs
removeEscapes _ = ""

infer :: String -> Type
infer str | isString str = Type.String $ removeEscapes str
infer str | isNumeric str =  parseNum str
infer str | isSymbol str = Type.Symbol str
infer str | isList str = Type.List $ parseList str
infer str 
    | str == "#t" = Type.Boolean True
    | str == "#f" = Type.Boolean False
infer str = throw $ UnknownType str