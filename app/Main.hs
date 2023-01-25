module Main where


consume :: [Char] -> [String] -> [String]
consume [] ys = ys
consume xs ys = parser xs ys

parser :: [Char] -> [String] -> [String]
parser (x:[]) ys = consume [] [[x]] <> ys
parser (')':xs) ys = consume xs [")"] <> ys
parser (x:xs) ys = parser xs [[x]] <> ys

main :: IO ()
main = do
    putStrLn ((parser "(2 (3 3))" [""]))
