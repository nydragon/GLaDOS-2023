module Parsing (Parser) where

-- ─── Data Structures ─────────────────────────────────────────────────────────────────────────────

-- Type representing a parsed value
-- a is the type of the value
-- Constructs Maybe value of tuple containing parsed a, and rest of string
type Parser a = String -> Maybe (a, String)

-- ─── Bootstrap Stuff ─────────────────────────────────────────────────────────────────────────────

parseChar :: Char -> Parser Char
parseChar c (x:xs)
    | c == x = Just (c, xs)
    | otherwise = Nothing
parseChar c arr = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar (x:xs) str = case res of
    Nothing -> parseAnyChar xs str
    Just _ -> res
    where res = parseChar x str
parseAnyChar [] _ = Nothing

-- parseOr :: (String -> Maybe (a, String)) -> (String -> Maybe (a, String)) -> (String -> Maybe (a, String))

-- parseOr parser_a parser_b
--     | parser_a 
