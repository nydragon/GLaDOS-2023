{-# LANGUAGE InstanceSigs #-}

module Instruction where

-- This type represents a single instruction
--
-- Aside from the conditional, each element of this type should correspond
-- to a single line in the output
data Instruction
    = Push String
    | Pop String
    | Call String
    | Init String
    | Move String String
    | Conditional [Instruction] [Instruction] [Instruction]

-- ─── Show Implementation ─────────────────────────────────────────────────────────────────────────

printInstructionList :: [Instruction] -> String
printInstructionList arr = unlines (map show arr)

instance Show Instruction where
    show :: Instruction -> String
    show (Push a) = "push " ++ a
    show (Pop a) = "pop " ++ a
    show (Call a) = "call " ++ a
    show (Init a) = "init " ++ a
    show (Move a b) = "move " ++ a ++ " " ++ b
    show (Conditional cond arr1 arr2) = printInstructionList cond ++ "if #RET\n" ++
        printInstructionList arr1 ++ "else\n" ++
        printInstructionList arr2 ++ "enif\n"