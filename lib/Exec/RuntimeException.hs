module Exec.RuntimeException where

import Control.Exception
import Exec.Lookup

-- Runtime exception data type
--
-- NOTE: This definition will be completed as development continues
data RuntimeException = InvalidFunctionCall String
    | UndefinedBehaviour -- Pretty much for anything that shouldn't ever happen
    | NotYetImplemented

    -- args: argument index, expected type, got type
    | InvalidArgument Integer String String
    | NullDivision
    deriving (Show, Eq)

instance Exception RuntimeException