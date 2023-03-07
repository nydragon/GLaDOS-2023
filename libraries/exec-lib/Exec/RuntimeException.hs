{-# LANGUAGE InstanceSigs #-}

module Exec.RuntimeException where

import Control.Exception

-- Runtime exception data type
--
-- NOTE: This definition will be completed as development continues
data RuntimeException
  = InvalidFunctionCall String
  | InvalidFunctionDefinition String
  | UndefinedBehaviour -- Pretty much for anything that shouldn't ever happen
  | Unimplemented
  -- args: argument index, expected type, got type
  | InvalidArgument Integer String String
  | InvalidArgumentCount String
  | NullDivision
  | FatalError -- For stuff that shouldn't happen
  | AlreadyDefined
  | NotAnInstruction String
  | UnknownType String
  deriving (Eq)

instance Exception RuntimeException

instance Show RuntimeException where
  show :: RuntimeException -> String
  show (InvalidFunctionCall a) = "InvalidFunctionCall: " ++ a ++ " is not a function."
  show UndefinedBehaviour = "UndefinedBehaviour: This behaviour is undefined."
  show Unimplemented = "Unimplemented"
  show (InvalidArgument a b c) = "InvalidArgument: argument " ++ show a ++ ", expected '" ++ b ++ "' but received '" ++ c ++ "'."
  show (InvalidArgumentCount fn) = "InvalidArgumentCount: Function " ++ fn ++ " received an invalid amount of arguments."
  show NullDivision = "NullDivision: Did you not pay attention in math class?"
  show FatalError = "FatalError"
  show (NotAnInstruction msg) = "NotAnInstruction: " ++ msg
  show AlreadyDefined = "AlreadyDefined"
  show (UnknownType val) = "UnknownType: Type couldn't be inferred " ++ val

  show _ = "oh oh"