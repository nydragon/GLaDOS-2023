module Compilation.RetVal where

import Instruction
import FunctionBlock

-- Used in order to return updated function definitions
--      as well as compiled instructions for the "current" expression
data RetVal = RetVal [Instruction] [FunctionBlock]

-- This function concatenates the arrays inside two retval
concatRetVal :: RetVal -> RetVal -> RetVal
concatRetVal (RetVal instr1 func1) (RetVal instr2 func2) = RetVal newInstrs newFuncs
    where
        newInstrs = instr1 ++ instr2
        newFuncs = func1 ++ func2