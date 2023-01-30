module Parsing where

-- Split on given character
-- Note: Uses parametric typing so can also work with integers ie
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c arr = (take nextSep arr) : splitOn c (drop nextSep arr)
    where nextSep = (getNextSep c arr 0) + 1 -- +1 because it's a character count

-- Gets index of next occurence of separator
getNextSep :: (Eq a) => a -> [a] -> Int -> Int
getNextSep _ [] i = i - 1 -- -1 because will have been incremented in recursive call
getNextSep t (x:xs) i
    | t == x = i
    | otherwise = getNextSep t xs (i + 1)