{-# LANGUAGE InstanceSigs #-}

module Compilation.FunctionBlock where

import Compilation.Instruction

data FunctionBlock = FunctionBlock String [Instruction]

instance Show FunctionBlock where
    show :: FunctionBlock -> String
    show (FunctionBlock name instructions) = "func " ++ name ++ "\n" ++ instructionStr "end\n"
        where
            instructionStr = showList instructions