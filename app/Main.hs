module Main where




tokenize :: [Char] -> String -> [String]
tokenize ('(':xs) b
        | length b == 0 = "(" : tokenize xs ""
        | otherwise = b : tokenize ('(':xs) ""
tokenize (')':xs) b = b: ")" : tokenize xs ""
tokenize (' ':xs) b = b : tokenize xs ""
tokenize [] b = []
tokenize (x:xs) b = tokenize xs (b <> [x])



main :: IO ()
main = do
    let tokenizedcode = tokenize "(define x 2)\n(add x 3)" ""

    print tokenizedcode
    
    
