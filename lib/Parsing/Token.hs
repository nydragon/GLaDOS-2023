module Parsing.Token where

import Data.Maybe
import Text.Read
import Data.Char (isDigit)
import Debug.Trace

-- ─── Tokenization ────────────────────────────────────────────────────────────────────────────────

-- Token Datatype
data Token = OpenScope -- Opening parenthesis
        | CloseScope -- Closing parenthesis
        | Num Integer
        | Keyword String
        | Literal String
        deriving (Show, Eq)

-- Parse token from string
parseToken :: String -> Token
parseToken "(" = OpenScope
parseToken ")" = CloseScope
parseToken input
        | all isDigit input = Num (read input :: Integer)
        | not (null (tail input)) && all isDigit (tail input) && head input == '-' = Num $ negate (read (tail input) :: Integer)
parseToken input = Keyword input


parseString' :: String -> String
parseString' ('\\' : x : xs ) = x : parseString' xs
parseString' ('"' : xs) = ""
parseString' (x : xs) = x : parseString' xs

parseString :: String -> Token
parseString ('"' : str) = Literal $ parseString' str

stringFastForward :: String -> String
stringFastForward ('\\' : x : xs ) = stringFastForward xs
stringFastForward ('"' : xs) = xs
stringFastForward (x : xs) = stringFastForward xs

-- Function that tokenizes string
--
-- Tokens are : ' ', '\n', '(', ')'
-- Args are : Input -> Temp Str -> Output List
tokenize' :: String -> String -> [Token]
tokenize' [] "" = []
tokenize' [] str = [parseToken str]
tokenize' (' ':xs) "" = tokenize' xs ""
tokenize' (' ':xs) str = parseToken str : tokenize' xs ""
tokenize' (',':xs) str = parseToken str : tokenize' xs ""
tokenize' ('\n':xs) "" = tokenize' xs ""
tokenize' ('\n':xs) str = parseToken str : tokenize' xs ""
tokenize' ('(':xs) "" = OpenScope : tokenize' xs ""
tokenize' ('(':xs) str = parseToken str : tokenize' ('(':xs) ""
tokenize' (')':xs) "" = CloseScope : tokenize' xs ""
tokenize' (')':xs) str = parseToken str : tokenize' (')':xs) ""
tokenize' ('[':xs) "" = OpenScope : tokenize' xs ""
tokenize' ('[':xs) str = parseToken str : tokenize' ('(':xs) ""
tokenize' (']':xs) "" = CloseScope : tokenize' xs ""
tokenize' (']':xs) str = parseToken str : tokenize' (')':xs) ""
tokenize' ('"':xs) "" = parseString ('"':xs) : tokenize' (stringFastForward xs) ""
tokenize' ('"':xs) str = parseToken str : parseString ('"':xs) : tokenize' (stringFastForward xs) ""
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