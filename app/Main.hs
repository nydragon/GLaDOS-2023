module Main where

tokenize' :: String -> String -> [String]
tokenize' [] str = []
tokenize' (' ':xs) str
        | length str == 0 = tokenize' xs ""
        | otherwise = str : tokenize' xs ""
tokenize' ('\n':xs) str
        | length str == 0 = tokenize' xs ""
        | otherwise = str : tokenize' xs ""
tokenize' ('(':xs) str
        | length str == 0 = "(" : tokenize' xs ""
        | otherwise = str : tokenize' ('(':xs) ""
tokenize' (')':xs) str
        | length str == 0 = ")" : tokenize' xs ""
        | otherwise = str : tokenize' (')':xs) ""
tokenize' (x:xs) str = tokenize' xs (str <> [x])

tokenize :: String -> [String]
tokenize str = tokenize' str ""

main :: IO ()
main = do
    let tokenizedcode = tokenize "(define x 2)\n\n( add    x 3 )"
    print tokenizedcode
