{-# LANGUAGE InstanceSigs #-}

module FunctionBlock where

import Instruction

data FunctionBlock = Function String [Instruction]

-- Removes first occurence of a main function block
-- (there should only be one anyways)
removeMainFunc :: [FunctionBlock] -> [FunctionBlock]
removeMainFunc (Function "main" _ : xs) = xs
removeMainFunc (x : xs) = x : removeMainFunc xs
removeMainFunc [] = []

instance Show FunctionBlock where
    show :: FunctionBlock -> String
    show (Function name instructions) = "func " ++ name ++ "\n" ++ instructionStr "end\n"
        where
            instructionStr = showList instructions