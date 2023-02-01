module Exec.RuntimeException where

import Control.Exception

-- Runtime exception data type
--
-- NOTE: This definition will be completed as development continues
data RuntimeException = InvalidFunctionCall String
    | UndefinedBehaviour -- Pretty much for anything that shouldn't ever happen
    | NotYetImplemented
    deriving (Show, Eq)

instance Exception RuntimeException