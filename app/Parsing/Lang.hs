module Parsing.Lang where

import Text.Read
import Data.Char

-- ─── Tokenization ────────────────────────────────────────────────────────────────────────────────

-- Token Datatype
data Token = OpenScope -- Opening parenthesis
        | CloseScope -- Closing parenthesis
        | Num Integer
        | Keyword String
        | Negation -- Minus
        deriving (Show)

-- Parse token from string
parseToken :: String -> Token
parseToken "(" = OpenScope
parseToken ")" = CloseScope
parseToken "-" = Negation
parseToken input
        | isNum input == True = Num (read input :: Integer)
        | otherwise = Keyword input

-- Function that tokenizes string
--
-- Tokens are : ' ', '\n', '(', ')'
-- Args are : Input -> Temp Str -> Output List
tokenize' :: String -> String -> [Token]
tokenize' [] str = []
tokenize' (' ':xs) str
        | null str = tokenize' xs ""
        | otherwise = (parseToken str) : tokenize' xs ""
tokenize' ('\n':xs) str
        | null str = tokenize' xs ""
        | otherwise = (parseToken str) : tokenize' xs ""
tokenize' ('(':xs) str
        | null str = OpenScope : tokenize' xs ""
        | otherwise = (parseToken str) : tokenize' ('(':xs) ""
tokenize' (')':xs) str
        | null str = CloseScope : tokenize' xs ""
        | otherwise = (parseToken str) : tokenize' (')':xs) ""
tokenize' ('-':xs) str
        | null str = Negation : tokenize' xs ""
        | otherwise = (parseToken str) : tokenize' ('-':xs) ""
tokenize' (x:xs) str = tokenize' xs (str <> [x])

-- Utility entry point function
tokenize :: String -> [Token]
tokenize str = tokenize' str ""

-- Function to tokenize a given file
--
-- Args : path
tokenizeFile :: String -> IO [Token]
tokenizeFile path = do
        -- Read file and Tokenize
        fileStr <- readFile path

        -- Return tokenization result
        return (tokenize fileStr)

-- ─── Utilities ───────────────────────────────────────────────────────────────────────────────────

isNum :: String -> Bool
isNum [] = True
isNum (x:xs) = if isDigit x then isNum xs else False