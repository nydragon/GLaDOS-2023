module Parsing where

-- ─── Parsing Utilities ───────────────────────────────────────────────────────────────────────────

-- Split on given character
-- Note: Uses parametric typing so can also work with integers ie
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c arr = take nextSep arr : splitOn c (drop nextSep arr)
    where nextSep = getNextSep c arr 0 + 1 -- +1 because it's a character count

-- Gets index of next occurence of separator
getNextSep :: (Eq a) => a -> [a] -> Int -> Int
getNextSep _ [] i = i - 1 -- -1 because will have been incremented in recursive call
getNextSep t (x:xs) i
        | t == x = i
        | otherwise = getNextSep t xs (i + 1)

-- ─── Tokenization ────────────────────────────────────────────────────────────────────────────────

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
