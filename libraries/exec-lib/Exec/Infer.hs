module Exec.Infer where
import qualified Exec.InferType as Type
import Exec.InferType (Type)
import Exec.Utils (parseNum)
import Data.Char (isAlpha)
import Utils (isValidBuiltin, isNumeric)
import Control.Exception (throw)
import Exec.RuntimeException (RuntimeException(Unimplemented))

isString :: String -> Bool
isString str | head str == '"' && last str == '"' = True
isString _ = False

isSymbol :: String -> Bool
isSymbol str | all isAlpha str || isValidBuiltin str = True
isSymbol _ = False

isList :: String -> Bool
isList = throw Unimplemented

parseList :: String -> Type
parseList = throw Unimplemented

infer :: String -> Type
infer str | isString str = Type.String str
infer str | isNumeric str =  parseNum str
infer str | isSymbol str = Type.Symbol str
infer str | isList str = parseList str