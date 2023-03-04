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

instance Show Instruction where
    showList :: [Instruction] -> ShowS
    showList [] = shows ""
    showList (x:xs) = (show x ++)

    show :: Instruction -> String
    show (Push a) = "push " ++ a
    show (Pop a) = "pop " ++ a
    show (Call a) = "call " ++ a
    show (Init a) = "init " ++ a
    show (Move a b) = "move " ++ a ++ " " ++ b
    show (Conditional cond arr1 arr2) = condStr "if #RET\n" ++ arr1Str "else\n" ++ arr2Str "enif\n"
        where
            condStr = showList cond
            arr1Str = showList arr1
            arr2Str = showList arr2