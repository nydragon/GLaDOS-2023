module Exec where

import Parsing.Ast

-- ─── Function And Variable Structures ────────────────────────────────────────────────────────────

type Func = (String, ExprList)
type Variable = (String, Num)

-- ─── Function Lookup ─────────────────────────────────────────────────────────────────────────────

lookupFunc 

-- ─── Main Function ───────────────────────────────────────────────────────────────────────────────

-- This is the main function used in order to evaluate an Aast
--
-- This function implements EXCEPTION HANDLING, hence the IO ()
--
-- Args : input ast -> Functions -> Variables
run' :: [Ast] -> [[Func]] -> 

-- Entry point function
run :: [Ast] -> IO ()
run [] = return ()
run (Call sym ls : xs) = 