module Parsing.Lang where

import Text.Read
import Data.Char

-- ─── Tokenization ────────────────────────────────────────────────────────────────────────────────

-- Token Datatype
data Token = OpenScope -- Opening parenthesis
        | CloseScope -- Closing parenthesis
        | Num Integer
        | Keyword String
        deriving (Show)

-- Parse token from string
parseToken :: String -> Token
parseToken "(" = OpenScope
parseToken ")" = CloseScope
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
tokenize' ('-':xs) [] = tokenize' xs "-"
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

-- ─── Concrete Parsing Tree ───────────────────────────────────────────────────────────────────────

-- Basic concrete parsing tree structure
-- Represents the main elements of our Context Free Grammar
data Cpt = Val Integer -- Using "Val" in order to avoid ambiguity check (yes it's ugly)
        | Sym String -- Symbol
        | List [Cpt] -- The list is sort of what an expression would be in other grammars

-- ─── Utilities ───────────────────────────────────────────────────────────────────────────────────

isNum :: String -> Bool
isNum [] = True
isNum (x:xs) = if isDigit x then isNum xs else False

getSymbol :: Cpt -> Maybe String
getSymbol (Sym s) = Just s
getSymbol _ = Nothing

getInteger :: Cpt -> Maybe Integer
getInteger (Val i) = Just i
getInteger _ = Nothing

getList :: Cpt -> Maybe [Cpt]
getList (List ls) = Just ls
getList _ = Nothing