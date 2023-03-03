module Parsing.TokenType where

-- Token Datatype
data Token = OpenScope -- Opening parenthesis
        | CloseScope -- Closing parenthesis
        | Num Integer
        | Flt Float
        | Keyword String
        | Literal String
        deriving (Show, Eq)
