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
    deriving (Eq)

-- ─── Show Implementation ─────────────────────────────────────────────────────────────────────────

showArr :: (a -> ShowS) -> [a] -> ShowS
showArr _ [] s = ""
showArr showx (x:xs) s = showx x (showl xs)
  where
    showl [] = s
    showl (y:ys) = showx y (showl ys)

instance Show Instruction where
    showList :: [Instruction] -> ShowS
    showList = showArr shows

    show :: Instruction -> String
    show (Push a) = "push " ++ a ++ "\n"
    show (Pop a) = "pop " ++ a ++ "\n"
    show (Call a) = "call " ++ a ++ "\n"
    show (Init a) = "init " ++ a ++ "\n"
    show (Move a b) = "move " ++ a ++ " " ++ b ++ "\n"
    show (Conditional cond arr1 arr2) =  "if #RET\n" ++ condStr "then\n" ++ arr1Str "else\n" ++ arr2Str "enif\n"
        where
            condStr = showList cond
            arr1Str = showList arr1
            arr2Str = showList arr2