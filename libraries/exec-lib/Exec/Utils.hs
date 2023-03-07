module Exec.Utils where

import Exec.InferType (Type, Registers)
import qualified Exec.InferType as Type
import Data.Map (insert, lookup)
import Utils

parseNum :: String -> Type
parseNum str | isPositiveInt str = Type.Integer (read str :: Integer)
parseNum str | isNegativeInt str =  Type.Integer $ -(read (tail str) :: Integer)
parseNum str | isPositiveFloat str = Type.Float (read str :: Float)
parseNum str | isNegativeFloat str = Type.Float $ -(read (tail str) :: Float)

-- Assign value Type to #RET
assignRet :: Type -> Registers -> Registers
assignRet = insert (Type.Symbol "#RET")

lookupRet :: Registers -> Maybe Type
lookupRet = Data.Map.lookup (Type.Symbol "#RET")