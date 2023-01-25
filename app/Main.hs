module Main where




tokenize :: String -> String -> [String]
tokenize [] b = []
tokenize (' ':xs) b
        | length b == 0 = tokenize xs ""
        | otherwise = b : tokenize xs ""
tokenize ('\n':xs) b
        | length b == 0 = tokenize xs ""
        | otherwise = b : tokenize xs ""
tokenize ('(':xs) b
        | length b == 0 = "(" : tokenize xs ""
        | otherwise = b : tokenize ('(':xs) ""
tokenize (')':xs) b
        | length b == 0 = ")" : tokenize xs ""
        | otherwise = b : tokenize (')':xs) ""
tokenize (x:xs) b = tokenize xs (b <> [x])



main :: IO ()
main = do
    let tokenizedcode = tokenize "(define x 2)\n\n( add    x 3 )" ""
    print tokenizedcode
    
    
