module Exec.Infer where
import qualified Exec.InferType as Type
import Exec.InferType (Type)
import Exec.Utils (parseNum, isNumeric)
import Data.Char (isAlpha)
import Utils (isValidBuiltin)


isString :: String -> Bool
isString str | head str == '"' && last str == '"' = True
isString _ = False

isSymbol :: String -> Bool
isSymbol str | all isAlpha str || isValidBuiltin str = True
isSymbol _ = False

-- isList :: String -> Bool

infer :: String -> Type
infer str | isString str = Type.String str
infer str | isNumeric str =  parseNum str
infer str | isSymbol str = Type.Symbol str
-- infer str | isList str = Type.List str