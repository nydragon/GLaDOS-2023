module Compilation.Registry where

-- This type will be used in order to keep track of:
--   Function names
--   Variable names
type Registry = ([String], [String])

-- ─── Utility Functions ──────────────────────────────────────────────────────────────────────────

isFunction :: String -> Registry -> Bool
isFunction funcName (funcs, _) = funcName `elem` funcs

isVariable :: String -> Registry -> Bool
isVariable varName (_, vars) = varName `elem` vars

isDeclared :: String -> Registry -> Bool
isDeclared name reg = isFunction name reg || isVariable name reg
