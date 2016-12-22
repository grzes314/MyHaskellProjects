{-
  Grzegorz Łoś
  Moduł zawiera abstrakcyjną reprezentację maszyny stanów, "pretty-printer"
  (funkcja showCharacter), funkcje obliczającą rozmiar maszyny (sizeCharacter).
-}

module Machine (
  Character(..),
  Rule(..),
  Stmt(..),
  ElseIf(..),
  Arm(..),
  Cond(..),
  Var(..),
  StateNum,
  NewState,
  Utterance,
  ValueSet,
  showCharacter,
  sizeCharacter,
  sizeStmt,
  armsSpan
) where

{-------------------------------------------------------------------------------
-------------------------- Definicja drzewa rozbioru ---------------------------
-------------------------------------------------------------------------------}

data Character = Character [Rule]
data Rule = Rule [StateNum] Stmt
data Stmt = If Cond Stmt [ElseIf] Stmt
           | Dec NewState Utterance
           | Case Var [Arm] Stmt
data ElseIf = ElseIf Cond Stmt
data Arm = Arm ValueSet Stmt
data Cond = Eq Var Integer
           | And [Cond]
           | Or [Cond]
data Var = Var String
type StateNum = Integer
type NewState = Maybe StateNum
type Utterance = String
type ValueSet = [StateNum]


{-------------------------------------------------------------------------------
---------------------- Konwersja maszyny do stringa ----------------------------
-------------------------------------------------------------------------------}

spaces :: Int -> String
spaces n = take n $ repeat ' '

showCharacter depth (Character rules) = "(\n" ++ concat (map (showRule (depth+2)) rules) ++ ")\n"

showRule depth (Rule snums stmt) =
  spaces depth ++ "( " ++ showSNums snums ++ "\n" ++ showStmt (depth+2) stmt
    ++ spaces depth ++ ")\n"
    where
      showSNums :: [StateNum] -> String
      showSNums [] = ""
      showSNums (x:xs) = showStateNum x ++ " " ++ showSNums xs



showStmt depth (If cond thenn elsifs els) =
  case elsifs of
    [] -> spaces depth ++ "(IF " ++ showCond cond ++ "\n" ++ showStmt (depth+4) thenn
          ++ spaces (depth+4) ++ "()\n" ++ showStmt (depth+4) els
          ++ spaces depth ++ ")\n"
    _ -> spaces depth ++ "(IF " ++ showCond cond ++ "\n" ++
              showStmt (depth+4) thenn ++ spaces (depth+4) ++ "(\n" ++
              concat (map (showElseIf (depth+6)) elsifs) ++ spaces (depth+4) ++ ")\n" 
              ++ showStmt (depth+4) els ++ spaces depth ++ ")\n"
showStmt depth (Dec ns utt) = spaces depth ++ "(DECISION " ++ showNewState ns ++ " "
  ++ showUtterance utt ++ ")\n"
showStmt depth (Case var arms def) =
  spaces depth ++ "(CASE " ++ showVar var ++ "\n" ++ spaces (depth+2) ++ "(\n" ++
    concat (map (showArm (depth+4)) arms) ++ spaces (depth+2) ++ ")\n"
    ++ showStmt (depth+2) def ++ spaces depth ++ ")\n"

showElseIf depth (ElseIf cond stmt) =
  spaces depth ++ "(ELSEIF " ++ showCond cond ++ "\n" ++ showStmt (depth+8) stmt
    ++ spaces depth ++ ")\n"

showArm depth (Arm vs stmt) =
  spaces depth ++ "(ARM " ++ showValueSet vs ++ "\n" ++ showStmt (depth+2) stmt
    ++ spaces depth ++ ")\n"

showCond (Eq var num) = "(EQUALS " ++ showVar var ++ show num ++ ") "
showCond (And conds) = "(AND " ++ concat (map showCond conds) ++ ") "
showCond (Or conds) = "(OR " ++ concat (map showCond conds) ++ ") "

showVar (Var name) = "(VAR \"" ++ name ++ "\") "

showStateNum :: StateNum -> String
showStateNum = show 

showNewState :: NewState -> String
showNewState Nothing = "_"
showNewState (Just sn) = showStateNum sn

showUtterance utt = "\"" ++ utt ++ "\""

showValueSet vs = "(" ++ showSNums vs ++ ") "
    where
      showSNums :: [StateNum] -> String
      showSNums [] = ""
      showSNums (x:xs) = showStateNum x ++ " " ++ showSNums xs

instance Show Character where
  show = showCharacter 0

     
{-------------------------------------------------------------------------------
---------------------- Obliczanie rozmiaru maszyny _----------------------------
-------------------------------------------------------------------------------}

sizeCharacter (Character rules) = sum $ map sizeRule rules

sizeRule (Rule _ stmt) = sizeStmt stmt

sizeStmt (If cond thenn elsifs els) = sizeCond cond + sizeStmt thenn +
  (sum $ map sizeElseIf elsifs) + sizeStmt els
sizeStmt (Dec ns utt) = case ns of
  Nothing -> 3
  _ -> 4
sizeStmt (Case var arms def) = let (s,b) = armsSpan arms
             in 10 + (b-s) + (sum $ map sizeArm arms) + sizeStmt def

sizeElseIf (ElseIf cond stmt) = sizeCond cond + sizeStmt stmt

sizeArm (Arm _ stmt) = sizeStmt stmt

sizeCond (Eq var n) = 6
sizeCond (And conds) = sum $ map sizeCond conds
sizeCond (Or conds) = sum $ map sizeCond conds

armsSpan :: [Arm] -> (Integer, Integer)
armsSpan arms = let labels (Arm vs _) = vs
                in (minimum (map (minimum . labels) arms),
                    maximum (map (maximum . labels) arms)) 
    



