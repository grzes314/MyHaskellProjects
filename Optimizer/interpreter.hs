{-
  Grzegorz Łoś
  Moduł zawiera interpreter maszyny stanów. 
-}

module Interpreter (
  Dict,
  Result,
  nextState
) where

import Machine

type Dict = [(String, Integer)]
type Result = (StateNum, String, Double)

{- Funkcja nextState przyjmuje maszynę stanów, obecny stan, słownik z
   wartościami zmiennych, a zwraca trójkę (nowy stan, komunikat, kosz ewaluacji)
-}
nextState :: Character -> StateNum -> Dict -> Result
nextState (Character rules) sn dict = selRule rules
  where
    selRule [] = (sn, "", 0)
    selRule ((Rule snums stmt) : rest) =
      if sn `elem` snums then evalStmt stmt sn dict 0
      else selRule rest

evalStmt (If cond thenn elsifs els) sn dict acc =
  let (b, cost) = evalCond cond dict in
    if b then evalStmt thenn sn dict (acc+cost)
    else case foldl check (Nothing,acc+cost) elsifs of
      (Nothing, accCost) -> evalStmt els sn dict accCost
      (Just result, _) -> result
    where
      check :: (Maybe Result, Double) -> ElseIf -> (Maybe Result, Double)
      check ((Just res), _) _ = (Just res, -1)
      check (Nothing, accCost) (ElseIf cond stmt) = let (b, cost2) = evalCond cond dict in
        if b then (Just (evalStmt stmt sn dict (accCost+cost2)), -1)
        else (Nothing, accCost+cost2)

evalStmt (Dec ns utt) sn dict acc = case ns of
     Just x -> (x, utt, acc+4)
     Nothing -> (sn, utt, acc+3)

evalStmt (Case var arms def) sn dict acc =
  let (Var name) = var in
  let (Just k) = lookup name dict in
  case foldl (check k) Nothing arms of
    Nothing -> evalStmt def sn dict (acc + 11.5)
    Just res -> res
  where      
      check k (Just res) _ = Just res
      check k Nothing (Arm vs stmt) = 
        if k `elem` vs then Just (evalStmt stmt sn dict (acc + 11.5))
        else Nothing

evalCond :: Cond -> Dict -> (Bool, Double)
evalCond (Eq var n) dict = 
  let (Var name) = var in
  let (Just k) = lookup name dict in
  (k == n, 6.5)

evalCond (And conds) dict = foldl fun (True, 0) conds where
  fun :: (Bool, Double) -> Cond -> (Bool, Double)
  fun (b, c) cond = if not b
                      then (False, c)
                      else let (b', c') = evalCond cond dict
                            in (b', c' + c)

evalCond (Or conds) dict = foldl fun (False, 0) conds where
  fun :: (Bool, Double) -> Cond -> (Bool, Double)
  fun (b, c) cond = if b then (True, c)
                    else let (b', c') = evalCond cond dict
                          in (b', c' + c)


