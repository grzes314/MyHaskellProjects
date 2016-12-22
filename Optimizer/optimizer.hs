{-
  Grzegorz Łoś
  Moduł optimizer zawiera funkcję służącą do optymalizowania maszyny.
-}

module Optimizer (
  module Machine,
  optimize
) where

import Machine
import Misc

type Dict = [(String, Integer)]
type Time = Int

{- Funkcja bierze Case'a i zwraca równoważną konstrukcje If -}
caseToIf (Case var arms def) =
  let vsToCond vs = Or $ map (\v -> Eq var v) vs
      armToElseIf (Arm vs stmt) = ElseIf (vsToCond vs) stmt
  in let ((Arm vs1 stmt1):rest) = arms --makeIf jest wywolywany tylko dla niepustej listy arms
     in If (vsToCond vs1) stmt1 (map armToElseIf rest) def
caseToIf x = x

{- Funkcja bierze warunek, o którym zakładamy, że jest prawdziwy, a następnie 
  stara się z niego wyciągnąć jak najwięcej (od teraz) znanych wartości
  zmiennych. -}
conclude :: Cond -> Dict
conclude trueCond = case trueCond of
  Eq (Var name) k -> [(name, k)]
  Or _ -> []
  And conds -> concatMap conclude conds

{- Funkcja przyjmuje warunek i słownik znanych wartości zmiennych. Jeżeli
  Znane wartości wystarczają do rozstrzygnięcia warunku, to zwracane jest
  Just True, lub Just False, w przeciwnym razie zwracane jest Nothing. -}
checkCond :: Cond -> Dict -> Maybe Bool
checkCond (Eq (Var name) k) dict = case lookup name dict of
  Nothing -> Nothing
  Just l -> Just $ k == l
checkCond (Or conds) dict = foldl fun (Just False) conds where
  fun (Just True) _ = Just True
  fun (Just False) cond = checkCond cond dict
  fun Nothing cond = case checkCond cond dict of
    Nothing -> Nothing
    (Just False) -> Nothing
    (Just True) -> Just True
checkCond (And conds) dict = foldl fun (Just True) conds where
  fun (Just False) _ = Just False
  fun (Just True) cond = checkCond cond dict
  fun Nothing cond = case checkCond cond dict of
    Nothing -> Nothing
    (Just False) -> Just False
    (Just True) -> Nothing

{- Funkcja simplifyCond stara sie uproscic podany warunek przy wiedzy zawartej
  w słowniku. Jeśli wyrażenie uprasza się do boola, to zwracamy Left True
  lub Left False. W przeciwym razie zwraca Right simCond gdzie simCond
  jest uproszczonym warunkiem. -} 
simplifyCond :: Cond -> Dict -> Either Bool Cond
simplifyCond (Eq (Var name) k) dict = case lookup name dict of
  Nothing -> Right (Eq (Var name) k)
  Just l -> Left $ k == l

simplifyCond (Or conds) dict = 
  let list = foldl fun (Left False) conds where
      fun (Left True) _ = Left True
      fun (Left False) cond = case simplifyCond cond dict of
        Left False -> Left False
        Left True -> Left True
        Right simCond -> Right [simCond]
      fun (Right simConds) cond = case simplifyCond cond dict of
        Left False -> (Right simConds)
        Left True -> Left True
        Right simCond -> Right $ simCond:simConds
  in case list of
       Left x -> Left x
       Right l -> Right $ Or l

simplifyCond (And conds) dict = 
  let list = foldl fun (Left True) conds where
      fun (Left False) _ = Left False
      fun (Left True) cond = case simplifyCond cond dict of
        Left False -> Left False
        Left True -> Left True
        Right simCond -> Right [simCond]
      fun (Right simConds) cond = case simplifyCond cond dict of
        Left False -> Left False
        Left True -> (Right simConds)
        Right simCond -> Right $ simCond:simConds
  in case list of
       Left x -> Left x
       Right l -> Right $ And l



getCondVars :: Cond -> [(String, [Integer])] -> [(String, [Integer])]
getCondVars (Eq (Var name) k) list = case lookup name list of
  Nothing -> (name, [k]):list
  Just vals -> (name, k:vals):(del name list) where
    del _ [] = []
    del name ((n, vs):tail) = if n == name then tail
                                            else (n,vs) : (del name tail)
getCondVars (Or conds) list = foldl (flip getCondVars) list conds
getCondVars (And conds) list = foldl (flip getCondVars) list conds


getIfVars (If cond stmt elsifs _) =
  let fun list (ElseIf c s) = getCondVars c list
  in  foldl fun (getCondVars cond []) elsifs

varName (Var name) = name

{- Funkcja optimize przyjmuje wejściową maszynę stanów, czas w jakim ma się
   zakończyć optymalizacja (int z milisekundami), a zwraca zoptymalizowaną
   maszynę.
-}
optimize :: Character -> Time -> Character

optimize (Character rules) _ = Character $ map optimizeRule rules

optimizeRule (Rule sns stmt) = Rule sns (optimizeStmt stmt [])


optimizeStmt :: Stmt -> Dict -> Stmt

optimizeStmt (Dec st utt) _ = Dec st utt

{- Optymalizacja Case'a:
  1. Jeżeli z wcześniejszych warunków znamy wartość zmiennej, po której jest
     case, to wybieramy odpowiednie ramię lub default, jeżeli wartość zmiennej
     nie należy do żadnego value-set. Zoptymalizowaną instrukcję z ramienia 
     (lub defaulta) zwracamy jako wynik.
  2. Jeżeli wartość zmiennej nie jest znana, to optymalizujemy instrukcje we
     wszystkich ramionach. Następnie tworzymy zoptymalizowanego case'a, po czym
     tworzymy równoważną instrukcję warunkową if i jako wynik zwracamy case'a
     lub ifa, w zależności od tego, który z nich ma mnieszy rozmiar.
-}
optimizeStmt (Case (Var name) arms def) dict =
  let makeNewArm (Arm vs stmt) = case vs of
         [] -> Nothing
         [a] -> Just $ Arm vs (optimizeStmt stmt ((name, a):dict))
         _ -> Just $ Arm vs (optimizeStmt stmt dict)
      useOneArm (Arm vs stmt) = optimizeStmt stmt dict
      useNewArms arms2 = let def2 = optimizeStmt def dict
                       in case arms2 of
            [] -> def2
            _ -> let case2 = Case (Var name) arms2 def2;
                     iff = caseToIf case2
                     costCase = sizeStmt case2
                     costIf = sizeStmt iff
                 in if costCase < costIf then case2 else iff
      findArm _ (Just arm) _ = Just arm
      findArm k Nothing (Arm vs thenn) = if k `elem` vs
                                        then Just (Arm vs thenn)
                                        else Nothing
      appendArm [] acc = reverse acc
      appendArm (h:t) acc = case makeNewArm h of
        Nothing -> appendArm t acc
        Just newArm -> appendArm t (newArm:acc)
  in case lookup name dict of
       Just k -> case foldl (findArm k) Nothing arms of
          Nothing -> optimizeStmt def dict
          Just arm -> useOneArm arm
       Nothing -> useNewArms (appendArm arms [])

{- Optymalizacja Ifa polega na uproszczeniu warunkow. Jesli jakis warunek jest
  prawdziwy to nie ma sensu pisac dalszych elsifow. Jesli jakis warunek jest
  nieprawdziwy to mozemy odrzucic tego elsifa. Jesli odrzucilismy wszystkie
  warunki to upraszczamy else'a i jego zwracamy jako wynik. W przeciwnym razie
  Probujemy powstalego ifa zmienic w case'a i jego upraszczamy
  -}
optimizeStmt (If cc ss ee dd) dictionary = 
  let simElseIf (ElseIf cond thenn) dict = case simplifyCond cond dict of
        Right simCond -> Right (ElseIf simCond thenn)
        Left x -> Left x

      -- jesli drugi element pary to Just cośtam, to znaleziono elsifa ze
      -- spelnionym warunkiem
      appElseIf _ (newElsIfs, Just stmt) _ = (newElsIfs, Just stmt)
      appElseIf dict (newElsIfs, Nothing) (ElseIf c s) = case simElseIf (ElseIf c s) dict of
        Right newEIf -> (newEIf : newElsIfs, Nothing)
        Left True -> (newElsIfs, Just s) -- ten elseif miał spełniony warunek
        Left False -> (newElsIfs, Nothing) --odrzucamy elseifa

      -- zwroci Right z "płytko" uproszczony ifem (jeżeli są jakieś warunki)
      -- lub Left z instrukcją (jeśli nie zostały żadne warunki)
      evalElseIfs eifs els dict = 
        let (revElsIfs, last) = foldl (appElseIf dict) ([], Nothing) eifs
            newElsIfs = reverse revElsIfs
        in case last of
          Nothing -> case newElsIfs of
             [] -> Left els
             ((ElseIf c s):tail) -> Right $ If c s tail els
          Just st -> case newElsIfs of
             [] -> Left st
             ((ElseIf c s):tail) -> Right $ If c s tail st

      shallow (If c s eifs def) dict =
        let elsifs' = (ElseIf c s):eifs
        in  evalElseIfs elsifs' def dict

      goDeep (If c s eifs def) dict = If c (optimizeStmt s (dict ++ conclude c))
        (map (\(ElseIf ec es) -> ElseIf ec $ optimizeStmt es (dict ++ conclude ec)) eifs)
        (optimizeStmt def dict)

      makeCase name vs stmt dict =
        let def = case shallow stmt ((name,-1720812993):dict) of
              Left x -> x
              Right x -> x
            arms = map (\v -> Arm [v] stmt) vs
        in optimizeStmt (Case (Var name) arms def) dict

      bestVar ((name, vals):rest) = best rest (name, vals) (length vals) where
        best [] pair _ = pair
        best ((n,vs):t) pair l = let newL = length vs
                                 in if newL > l then best t (n,vs) newL
                                                else best t pair l
  in case shallow (If cc ss ee dd) dictionary of
    Left stmt -> optimizeStmt stmt dictionary
    Right stmt ->
      let deep = goDeep stmt dictionary
          (varName, vs) = bestVar $ getIfVars stmt
          toCase = makeCase varName (remRep vs) stmt dictionary
          sizeDeep = sizeStmt deep
          sizeCase = sizeStmt toCase
       in if sizeDeep <= sizeCase then deep else toCase



