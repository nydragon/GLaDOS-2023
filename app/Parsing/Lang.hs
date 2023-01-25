module Parsing.Lang where

-- ─── Tokenization ────────────────────────────────────────────────────────────────────────────────

-- Token Datatype
data Token = OpenScope
        | CloseScope
        | Number Integer
        | Keyword String
        | OpenSingle -- single quotes
        | CloseSingle
        | OpenDouble -- double quotes
        | CloseDouble

-- Function that tokenizes string
--
-- Tokens are : ' ', '\n', '(', ')'
-- Args are : Input -> Temp Str -> Output List
tokenize' :: String -> String -> [String]
tokenize' [] str = []
tokenize' (' ':xs) str
        | null str = tokenize' xs ""
        | otherwise = str : tokenize' xs ""
tokenize' ('\n':xs) str
        | null str = tokenize' xs ""
        | otherwise = str : tokenize' xs ""
tokenize' ('(':xs) str
        | null str = "(" : tokenize' xs ""
        | otherwise = str : tokenize' ('(':xs) ""
tokenize' (')':xs) str
        | null str = ")" : tokenize' xs ""
        | otherwise = str : tokenize' (')':xs) ""
tokenize' (x:xs) str = tokenize' xs (str <> [x])

-- Utility entry point function
tokenize :: String -> [String]
tokenize str = tokenize' str ""

-- Function to tokenize a given file
--
-- Args : path
tokenizeFile :: String -> IO [String]
tokenizeFile path = do
        -- Read file and Tokenize
        fileStr <- readFile path

        -- Return tokenization result
        return (tokenize fileStr)