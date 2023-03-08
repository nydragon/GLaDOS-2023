module Compilation.Registry where

import Data.List

-- This type will be used in order to keep track of:
--   Function names
--   Variable names
type Registry = ([String], [String])

-- ─── Addition ────────────────────────────────────────────────────────────────────────────────────

addVar :: String -> Registry -> Registry
addVar s (f, v) = (f, s : v)

addVars :: [String] -> Registry -> Registry
addVars s (f, v) = (f, s ++ v)

addFunction :: String -> Registry -> Registry
addFunction s (f, v) = (s : f, v)

-- ─── Utility Functions ──────────────────────────────────────────────────────────────────────────

isFunction :: String -> Registry -> Bool
isFunction funcName (funcs, _) = funcName `elem` funcs

isVariable :: String -> Registry -> Bool
isVariable varName (_, vars) = varName `elem` vars

isDeclared :: String -> Registry -> Bool
isDeclared name reg = isFunction name reg || isVariable name reg

emptyRegistry :: Registry
emptyRegistry = ([], [])

combineRegistries :: Registry -> Registry -> Registry
combineRegistries reg1 reg2 = (f1 `union` f2, v1 `union` v2)
    where
        (f1, v1) = reg1
        (f2, v2) = reg2
