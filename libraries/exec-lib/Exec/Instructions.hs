module Exec.Instructions where
import Exec.InferType (Stack (Stack), Type, StackFrame (StackFrame))
import Data.Map (insert)
import qualified Exec.InferType as Type

pushVal :: Type -> Stack -> Stack
pushVal  v (Stack sf as reg) = Stack sf (v : as) reg

popVal :: Type -> Stack -> Stack
popVal v (Stack (sf:sfs) as reg) = Stack (newSf : sfs) rest reg
    where
        ([val], rest) = splitAt 1 as
        StackFrame lv a b = sf
        newSf = StackFrame (insert v val lv) a b