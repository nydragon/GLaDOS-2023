{-# LANGUAGE InstanceSigs #-}

module Exec.RuntimeException where

import Control.Exception
import Exec.Lookup

-- Runtime exception data type
--
-- NOTE: This definition will be completed as development continues
data RuntimeException
  = InvalidFunctionCall String
  | UndefinedBehaviour -- Pretty much for anything that shouldn't ever happen
  | NotYetImplemented
  | -- args: argument index, expected type, got type
    InvalidArgument Integer String String
  | NullDivision
  deriving (Eq)

instance Exception RuntimeException

instance Show RuntimeException where
  show :: RuntimeException -> String
  show (InvalidFunctionCall a) = a ++ " is an invalid function call."
  show UndefinedBehaviour = "This is undefined."
  show NotYetImplemented = "OH MAXI BEBOU"
  show (InvalidArgument a b c) = "Argument " ++ show a ++ " invalid: expected '" ++ b ++ "' but received '" ++ c ++ "'."
  show NullDivision = "Did you not pay attention in math class?"
