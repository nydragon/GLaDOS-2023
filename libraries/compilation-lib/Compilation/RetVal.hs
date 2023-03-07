module Compilation.RetVal where

import Instruction
import FunctionBlock
import Compilation.Registry

-- Used in order to return updated function definitions
--      as well as compiled instructions for the "current" expression
data RetVal = RetVal [Instruction] [FunctionBlock] Registry

-- This function concatenates the arrays inside two retval
concatRetVal :: RetVal -> RetVal -> RetVal
concatRetVal (RetVal instr1 func1 reg1) (RetVal instr2 func2 reg2) = RetVal newInstrs newFuncs newReg
    where
        newInstrs = instr1 ++ instr2
        newFuncs = func1 ++ func2
        newReg = combineRegistries reg1 reg2

prependInstructions :: RetVal -> [Instruction] -> RetVal
prependInstructions (RetVal instrs funcs reg) instructions = RetVal (instructions ++ instrs) funcs reg

appendInstructions :: RetVal -> [Instruction] -> RetVal
appendInstructions (RetVal instrs funcs reg) instructions = RetVal (instrs ++ instructions) funcs reg

appendFunctions :: RetVal -> [FunctionBlock] -> RetVal
appendFunctions (RetVal instrs funcs reg) func = RetVal instrs (funcs ++ func) reg