{-# LANGUAGE InstanceSigs #-}

module FunctionBlock where

import Instruction

data FunctionBlock = Function String [Instruction]

instance Show FunctionBlock where
    show :: FunctionBlock -> String
    show (Function name instructions) = "func " ++ name ++ "\n" ++ instructionStr "end\n"
        where
            instructionStr = showList instructions