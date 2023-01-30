module Parsing.Args where

import System.Environment
import System.Exit

-- ParsedArgs structure
-- Boolean true if flag found
data ParsedArgs = ParsedArgs {
    usage :: Bool,
    path :: String
} deriving (Show, Eq)

-- Args : Arg Array -> Counter -> Temp ParsedArgs -> Output
parseArgs' :: [String] -> Int -> ParsedArgs -> IO ParsedArgs -- Maybe only seems necessary for do notation

-- Positional args
parseArgs' (x:xs) 0 args = parseArgs' xs 1 args
parseArgs' ("-h":xs) c args = parseArgs' xs (c + 1) (args { usage = True })
parseArgs' (x:xs) 1 args = parseArgs' xs 2 (args { path = x })
parseArgs' _ 2 args = return args -- max args
-- Flags go here
parseArgs' (x:xs) c args = parseArgs' xs (c + 1) args
-- End condition
parseArgs' [] _ args = return args

-- Utility entry point function
--
-- Returns parsed file path
parseArgs :: IO ParsedArgs
parseArgs = getArgs >>= (\args -> parseArgs' args 0 (ParsedArgs False "")) -- skip binary name