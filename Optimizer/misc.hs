{-
  Grzegorz Łoś
  Różne funkcje przydatne do testowania.
-}

module Misc (
  module Machine,
  delAll,
  remRep,
  getVars,
  getMin,
  getMax,
  states
) where

import Machine

delAll :: (Eq a) => [a] -> a -> [a]
delAll [] _ = []
delAll (h:t) x = if h == x then delAll t x else h : delAll t x

remRep :: (Eq a) => [a] -> [a]
remRep [] = []
remRep (h:t) = let t' = delAll t h in h : remRep t'

-- Zwraca nazwy wszystkich zmiennych występujących w maszynie.
getVars :: Character -> [String]
getVars (Character rules) = remRep $ concat $ map varsRule rules

varsRule (Rule _ stmt) = varsStmt stmt

varsStmt (If cond thenn elsifs els) = varsCond cond ++ varsStmt thenn
  ++ (concat $ map varsElseIf elsifs) ++ varsStmt els
varsStmt (Dec ns utt) = []
varsStmt (Case var arms def) = (varName var) : (concat $ map varsArm arms)
  ++ varsStmt def

varsElseIf (ElseIf cond stmt) = varsCond cond ++ varsStmt stmt

varsArm (Arm _ stmt) = varsStmt stmt

varsCond (Eq var n) = [varName var]
varsCond (And conds) = concat $ map varsCond conds
varsCond (Or conds) = concat $ map varsCond conds

varName (Var str) = str

-- Zwraca wszystkie stany wyszczególnione na listach reguł.
states :: Character -> [StateNum]
states (Character rules) = remRep $ concatMap statesRule rules where
  statesRule (Rule snums _) = snums


infty = 1000000000

minimum' xs = case xs of
  [] -> infty
  _ -> minimum xs

maximum' xs = case xs of
  [] -> -infty
  _ -> maximum xs

{- Maszyna -> NazwaZmiennej -> Wynik, gdzie Wynik to
  minimalna liczba, która występuje przy podanej zmiennej w wyrażeniu CASE
  lub EQUALS. -}
getMin :: Character -> String -> Integer
getMin (Character rules) var = minimum' $ map (getMinRule var) rules

getMinRule var (Rule _ stmt) = getMinStmt var stmt

getMinStmt var (If cond thenn elsifs els) = minimum $ [getMinCond var cond, getMinStmt var thenn,
  minimum' (map (getMinElseIf var) elsifs), getMinStmt var els]
getMinStmt var (Dec ns utt) = infty
getMinStmt name (Case var arms def) =
  if (varName var) == name
    then minimum [m, s, getMinStmt name def]
    else min m (getMinStmt name def)
  where 
    m = minimum' $ map (getMinArm name) arms
    (s,_) = armsSpan arms

getMinElseIf var (ElseIf cond stmt) = min (getMinCond var cond) (getMinStmt var stmt)

getMinArm var (Arm _ stmt) = getMinStmt var stmt

getMinCond name (Eq var n) = if (varName var) == name then n else infty
getMinCond var (And conds) = minimum' $ map (getMinCond var) conds
getMinCond var (Or conds) = minimum' $ map (getMinCond var) conds


{- Maszyna -> NazwaZmiennej -> Wynik, gdzie Wynik to
  maksymalna liczba, która występuje przy podanej zmiennej w wyrażeniu CASE
  lub EQUALS. -}
getMax :: Character -> String -> Integer
getMax (Character rules) var = maximum' $ map (getMaxRule var) rules

getMaxRule var (Rule _ stmt) = getMaxStmt var stmt

getMaxStmt var (If cond thenn elsifs els) = maximum $ [getMaxCond var cond, getMaxStmt var thenn,
  maximum' (map (getMaxElseIf var) elsifs), getMaxStmt var els]
getMaxStmt var (Dec ns utt) = -infty
getMaxStmt name (Case var arms def) =
  if (varName var) == name
    then maximum [m, b, getMaxStmt name def]
    else max m (getMaxStmt name def)
  where 
    m = maximum' $ map (getMaxArm name) arms
    (_,b) = armsSpan arms

getMaxElseIf var (ElseIf cond stmt) = max (getMaxCond var cond) (getMaxStmt var stmt)

getMaxArm var (Arm _ stmt) = getMaxStmt var stmt

getMaxCond name (Eq var n) = if (varName var) == name then n else -infty
getMaxCond var (And conds) = maximum' $ map (getMaxCond var) conds
getMaxCond var (Or conds) = maximum' $ map (getMaxCond var) conds



