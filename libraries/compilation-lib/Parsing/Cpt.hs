module Parsing.Cpt where

import Parsing.TokenType
import Data.Maybe

-- ─── Concrete Parsing Tree ───────────────────────────────────────────────────────────────────────

-- Basic concrete parsing tree structure
-- Represents the main elements of our Context Free Grammar
data Cpt = Val Integer -- Using "Val" in order to avoid ambiguity check (yes it's ugly)
        | Floating Float
        | Literal' String
        | Sym String -- Symbol
        | List [Cpt] -- The list is sort of what an expression would be in other grammars
        | Boolean Bool
        deriving (Show, Eq)

-- The following functions all work on a piece of [Token]

-- Parses token
-- Returns nothing if : OpenScope, CloseScope => Should be handled by parseCptList
tokenToCpt :: Token -> Maybe Cpt
-- These cases shouldn't happen
tokenToCpt OpenScope = Nothing
tokenToCpt CloseScope = Nothing
-- Main cases
tokenToCpt (Num i) = Just (Val i)
tokenToCpt (Flt i) = Just (Floating i)
tokenToCpt (Literal i) = Just (Literal' i)
tokenToCpt (Keyword "#f") = Just (Boolean False)
tokenToCpt (Keyword "#t") = Just (Boolean True)
-- if a symbol starts and ends with ` it means that it is used in infix form
tokenToCpt (Keyword str) = Just (Sym str)

-- Parses list in between parenthesis
-- EXPECTS : First token in list to be OpenScope
--
-- NOTE: Returns List even if no corresponding closing parenthesis is found
--       this will potentially be an issue
--
-- Args : Tokens -> Output
parseTokenList :: [Token] -> [Cpt]
parseTokenList [] = []
parseTokenList (CloseScope:_) = [] -- Normally shouldn't happen
parseTokenList (OpenScope:xs) = sublist : parseTokenList newList
        where   sublist = List (parseTokenList xs)
                index = getCloseScope xs + 1
                -- +1 because it's index of CloseScope, and we want the element after
                newList = [x | (x, i) <- zip xs [0..], i >= index] -- NEEDS MORE TESTING
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
getCloseScope [] = 0
getCloseScope (_:xs) = getCloseScope' xs 1 0 1 -- index starts at 1 since first element is skipped

-- ─── Utilities ───────────────────────────────────────────────────────────────────────────────────

getSymbol :: Cpt -> Maybe String
getSymbol (Sym s) = Just s
getSymbol _ = Nothing

getInteger :: Cpt -> Maybe Integer
getInteger (Val i) = Just i
getInteger _ = Nothing

getList :: Cpt -> Maybe [Cpt]
getList (List ls) = Just ls
getList _ = Nothing