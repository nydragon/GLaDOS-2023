{-# LANGUAGE InstanceSigs #-}

module Compilation.Instruction where

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

showArr :: (a -> ShowS) -> [a] -> ShowS
showArr _ [] s = ""
showArr showx (x:xs) s = showx x (showl xs) ++ ['\n']
  where
    showl [] = s
    showl (y:ys) = '\n' : showx y (showl ys)

instance Show Instruction where
    showList :: [Instruction] -> ShowS
    showList = showArr shows

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