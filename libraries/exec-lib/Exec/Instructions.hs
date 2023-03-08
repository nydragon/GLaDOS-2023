module Exec.Instructions where
import Exec.InferType (Stack (Stack), Type, StackFrame (StackFrame), Registers)
import Data.Map (insert, lookup)
import Data.Maybe (fromJust)
import qualified Exec.InferType as Type
import Control.Exception (throw)
import Exec.RuntimeException (RuntimeException(FatalError))
import Exec.Utils (assignRet)
import Debug.Trace (trace)

pushVal :: Type -> Stack -> Stack
pushVal (Type.Symbol sym) (Stack sf as reg) = Stack sf (resolveVar (Type.Symbol sym) reg : as) reg
pushVal  v (Stack sf as reg) = Stack sf (v : as) reg

popVal :: Type -> Stack -> Stack
popVal v (Stack (sf:sfs) as reg) = Stack (newSf : sfs) rest reg
    where
        ([val], rest) = splitAt 1 as
        StackFrame lv a b = sf
        newSf = StackFrame (insert v val lv) a b
popVal _ _ = throw $ FatalError "popVal"


resolveVar :: Type -> Registers -> Type
resolveVar t reg = fromJust $ Data.Map.lookup t reg

initVar :: Type -> Stack -> Stack
initVar (Type.Symbol sym) (Stack sf as reg) = Stack sf as (insert (Type.Symbol sym) Type.Null reg)
initVar _ _ = throw $ FatalError "initVar"

moveVar :: Type -> Type -> Stack -> Stack 
moveVar (Type.Symbol "#RET") val (Stack sf as reg) = trace (show val)  Stack sf as (assignRet val reg)
moveVar (Type.Symbol sym) val (Stack sf as reg) = Stack sf as (insert (Type.Symbol sym) val reg)
moveVar _ _ _ = throw $ FatalError "moveVar"

