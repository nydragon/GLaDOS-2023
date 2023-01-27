module Parsing.Lang where

import Text.Read
import Data.Char
import Data.Maybe

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

-- ─── Concrete Parsing Tree ───────────────────────────────────────────────────────────────────────

-- Basic concrete parsing tree structure
-- Represents the main elements of our Context Free Grammar
data Cpt = Val Integer -- Using "Val" in order to avoid ambiguity check (yes it's ugly)
        | Sym String -- Symbol
        | List [Cpt] -- The list is sort of what an expression would be in other grammars
        deriving (Show)

-- The following functions all work on a piece of [Token]

-- Parses token
-- Returns nothing if : OpenScope, CloseScope => Should be handled by parseCptList
tokenToCpt :: Token -> Maybe Cpt
-- These cases shouldn't happen
tokenToCpt OpenScope = Nothing
tokenToCpt CloseScope = Nothing
-- Main cases
tokenToCpt (Num i) = Just (Val i)
tokenToCpt (Keyword str) = Just (Sym str)

-- Parses list in between parenthesis
-- EXPECTS : First token in list to be OpenScope
--
-- NOTE: Returns List even if no corresponding closing parenthesis is found
--       this will potentially be an issue
-- NOTE: Will need way of parsing "initial" list (ie: list without parenthesis)
--
-- Args : Tokens -> Output
parseTokenList :: [Token] -> [Cpt]
parseTokenList [] = []
parseTokenList (CloseScope:xs) = [] -- Normally shouldn't happen
parseTokenList (OpenScope:xs) = sublist : parseTokenList newList
        where   sublist = List (parseTokenList xs)
                index = getCloseScope xs +2
                -- +2 because it's index of CloseScope, and we want the element after
                newList = [x | (x, i) <- zip xs [0..], i >= index] -- NEEDS MORE TESTING
                -- newList = take (getCloseScope oldList + 2) oldList
parseTokenList (x:xs) = fromJust (tokenToCpt x) : parseTokenList xs
-- Note: If fromJust crashes, something else has gone wrong



-- Sublist needs to be list object but not of an array of Cpt
--
-- newList -> take doesn't work as planned

-- Gets CORRESPONDING closing parenthesis
-- This function takes into account any intermediate opening parenthesis
--
-- Args : Input list -> Opening Count -> Closing Count -> Index -> Output
getCloseScope' :: [Token] -> Int -> Int -> Int -> Int
getCloseScope' [] _ _ i = i - 1
getCloseScope' (OpenScope:xs) openCount closeCount i =
        getCloseScope' xs (openCount + 1) closeCount (i + 1)
getCloseScope' (CloseScope:xs) openCount closeCount i
        | openCount == closeCount + 1 = i
        | otherwise = getCloseScope' xs openCount (closeCount + 1) (i + 1)
getCloseScope' (_:xs) c1 c2 i = getCloseScope' xs c1 c2 (i + 1)


-- Utility entry point function
-- EXPECTS : First token in list to be OpenScope
getCloseScope :: [Token] -> Int
getCloseScope (x:xs) = getCloseScope' xs 1 0 1 -- index starts at 1 since first element is skipped

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

-- Refactor ideas :
-- Add list comprehension for list slicing in parseTokenList