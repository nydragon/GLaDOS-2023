data Cpt = Integer Integer
    | Symbol String
    | List [Cpt]
    deriving (Show, Eq, Ord)

division :: Integer -> Integer -> Either Integer String
division a 0 = Right "ZeroDivisionError"
division a b = Left (div a b)

modulo :: Integer -> Integer -> Either Integer String
modulo a 0 = Right "ZeroDivisionError"
modulo a b = Left (mod a b)

multiply :: Integer -> Integer -> Integer
multiply a b = a * b

subtract :: Integer -> Integer -> Integer
subtract a b = a - b

add :: Integer -> Integer -> Integer
add a b = a + b

lt :: (Ord a) => a -> a -> Bool
lt a b = a < b