module Parsing.Token where

import Data.Maybe
import Text.Read

-- ─── Tokenization ────────────────────────────────────────────────────────────────────────────────

-- Token Datatype
data Token = OpenScope -- Opening parenthesis
        | CloseScope -- Closing parenthesis
        | Num Integer
        | Keyword String
        deriving (Show, Eq)

-- Parse token from string
parseToken :: String -> Token
parseToken "(" = OpenScope
parseToken ")" = CloseScope
parseToken input = case readMaybe input :: Maybe Integer of
        Just x -> Num x
        Nothing -> Keyword input

-- Function that tokenizes string
--
-- Tokens are : ' ', '\n', '(', ')'
-- Args are : Input -> Temp Str -> Output List
tokenize' :: String -> String -> [Token]
tokenize' [] "" = []
tokenize' [] str = [parseToken str]
tokenize' (' ':xs) "" = tokenize' xs ""
tokenize' (' ':xs) str = parseToken str : tokenize' xs ""
tokenize' ('\n':xs) "" = tokenize' xs ""
tokenize' ('\n':xs) str = parseToken str : tokenize' xs ""
tokenize' ('(':xs) "" = OpenScope : tokenize' xs ""
tokenize' ('(':xs) str = parseToken str : tokenize' ('(':xs) ""
tokenize' (')':xs) "" = CloseScope : tokenize' xs ""
tokenize' (')':xs) str = parseToken str : tokenize' (')':xs) ""
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