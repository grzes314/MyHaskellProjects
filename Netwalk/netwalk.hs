import Prelude hiding (Right, Left)
   
type Configuration = [[Field]]
data Field = Wire [Direction] | Source [Direction] | PC Direction | Empty deriving (Read, Show)
data Direction = Up | Right | Down | Left deriving (Read, Show)
type Solution = [[Turn]]
type Turn = Int -- -1 .. 2
type DlugoscBoku = Int
data Pole = Pole Int Int

instance Eq Direction where
	Up == Up = True
	Right == Right = True
	Left  == Left = True
	Down == Down = True
	_ == _ = False
	
instance Ord Direction where
	Up `compare` Up = EQ
	Up `compare` Right = LT
	Up `compare` Down = LT
	Up `compare` Left = LT	
	Right `compare` Up = GT
	Right `compare` Right = EQ
	Right `compare` Down = LT
	Right `compare` Left = LT
	Down `compare` Up = GT
	Down `compare` Right = GT
	Down `compare` Down = EQ
	Down `compare` Left = LT
	Left `compare` Up = GT
	Left `compare` Right = GT
	Left `compare` Down = GT
	Left `compare` Left = EQ


get :: [[a]] -> Int -> Int -> a
get xss i j = (xss !! i) !! j

set :: [[a]] -> Int -> Int -> a -> [[a]] 
set (xs:xss) 0 j val = (set' xs j val) : xss
  where set' :: [b] -> Int -> b -> [b]
        set' (x:xs) 0 val = val:xs
        set' (x:xs) j val = x : ( set' xs (j-1) val)
set (xs:xss) i j val = xs : ( set xss (i-1) j val)

dajKierunki :: Field -> [Direction]
dajKierunki Wire xs = xs
dajKierunki Source xs = xs
dajKierunki PC x = [x]
dajKierunki Empty = []

plansza :: DlugoscBoku -> DlugoscBoku -> Int -> [[Int]]
plansza 0 _ _ = []
plansza n k val = (listaVal k val) : (plansza (n-1) k val)

listaVal :: DlugoscBoku -> Int -> [Int]
listaVal 0 val = []
listaVal n val = val:(listaVal (n-1) val)

showArr2D :: (Show a) => [[a]] -> String
showArr2D [] = " "
showArr2D (xs:xss) = show xs ++ "\n" ++ showArr2D xss
  
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lower ++ [x] ++ qsort higher
  where lower = [ y | y <- xs, y <= x]
        higher = [ y | y <- xs, y > x]


turnDir' :: Direction -> Direction -- w prawo
turnDir' Up = Right
turnDir' Right = Down
turnDir' Down = Left
turnDir' Left = Up

turnDir :: Int -> Direction -> Direction
turnDir 0 dir = dir
turnDir 1 dir = turnDir' dir
turnDir 2 dir = turnDir' (turnDir' dir)
turnDir (-1) dir = turnDir' ( turnDir' (turnDir' dir) )

turnAll :: Int -> Field -> Field
turnAll _ Empty = Empty
turnAll n (Wire xs) = Wire (qsort $ map (turnDir n) xs)
turnAll n (Source xs) = Source (qsort $ map (turnDir n) xs)
turnAll n (PC x) = PC (turnDir n x)

solve :: Configuration -> Int
-- konfiguracja -> liczbaRozwiazan
solve conf = solve' conf (plansza n n 0) n 0 0
  where n = length conf

solve' :: Configuration -> Solution -> DlugoscBoku -> Int -> Int -> Int
-- konfiguracja -> rozwiazanie -> dlugoscBoku -> obecnyWers -> obecnaKolumna -> liczbaRozwiazan
solve' conf sol n i j
	| i >= n = 0 -- if (sprawdz conf) then do putStrLn(show Solution) return 1 else 0
	| j >= n = solve' conf sol n (i+1) 0
	otherwise = (if (poprawnie conf1 n i j) then (solve' conf1 (set sol i j 0) n i (j+1)) else 0) +
	            (if (poprawnie conf2 n i j) then (solve' conf2 (set sol i j 1) n i (j+1)) else 0) +
	            (if (poprawnie conf3 n i j) then (solve' conf3 (set sol i j 2) n i (j+1)) else 0) +
	            (if (poprawnie conf4 n i j) then (solve' conf4 (set sol i j -1) n i (j+1)) else 0)
              where conf1 = set conf i j ( turnAll 0 (dajKierunki $ get conf i j))
                    conf2 = set conf i j ( turnAll 1 (dajKierunki $ get conf i j))
                    conf3 = set conf i j ( turnAll 2 (dajKierunki $ get conf i j))
                    conf4 = set conf i j ( turnAll -1 (dajKierunki $ get conf i j))

-- funkcja poprawnie bada czy element na polu i j jest ulozony poprawnie wzgledem pola wyzej i pola po lewej
-- p=>q <=> ~p v q
poprawnie :: Configuration -> DlugoscBoku -> Int -> Int -> Bool
poprawnie conf n i j = (if i==0 then not (elem Up (get conf i j)) else True) && --jesli jestesmy w pierwszym wierszu to nie moze byc ogonka w gore
                       (if i==n-1 then not (elem Down (get conf i j)) else True) && --jesli jestesmy w ostatnim wierszu to nie moze byc ogonka w dol
					   (if j==0 then not (elem Left (get conf i j)) else True) && --jesli jestesmy w pierwszej kolumnie to nie moze byc ogonka w lewo
                       (if j==n-1 then not (elem Right (get conf i j)) else True) && --jesli jestesmy w ostatniej kolumnie to nie moze byc ogonka w prawo
					   (if (i>0) then ( (elem Down (get conf (i-1) j)) `wtw` (elem Up (get conf i j)) ) else True) && --zeby stykaly sie ogonki w pionie
                       (if (j>0) then ( (elem Right (get conf i (j-1))) `wtw` (elem Left (get conf i j)) ) else True) //zeby sykaly ogonki w poziomie

wtw :: Bool -> Bool -> Bool
wtw True True = True
wtw False False = True
wtw _ _ = False

sprawdz :: Configuration -> DlugoscBoku -> Bool
sprawdz conf n = sprawdz''(conf (sprawdz' conf (findSource conf) (plansza n n 0)) n )

sprawdz' :: Configuration -> Pole -> [[Int]] -> [[Int]]
-- konfiguracja -> pole(do ktorego wlasnie doszedl internet) -> lista-akumulator (pusta poczatkowo) -> lista wynikowa
sprawdz' _ (Pole -1 _) plansza = plansza
sprawdz' _ (Pole n _) plansza = plansza
sprawdz' _ (Pole _ -1) plansza = plansza
sprawdz' _ (Pole _ n) plansza = plansza
sprawdz' conf (Pole i j) plansza = if (get plansza i j)==1 then plansza
                                   else if (get conf i j)==(PC _) then (set plansza i j 1)
								   else if (get conf i j)==(Wire xs) then nastepneKroki xs
								   else if (get conf i j)==(Source xs) then nastepneKroki xs
								   else plansza --to i tak sie nie zdarzy
								     where nastepneKroki :: [Direction] -> [[Int]]
									       nastepneKroki xs =
										     let plansza1 = set plansza i j 1
											     plansza2 = if (Left `elem` xs) then sprawdz' conf ((Pole i j) `go` Left) plansza1
										                    else plansza1
												 plansza3 = if (Down `elem` xs) then sprawdz' conf ((Pole i j) `go` Down) plansza2
										                    else plansza2
												 plansza4 = if (Right `elem` xs) then sprawdz' conf ((Pole i j) `go` Right) plansza3
										                    else plansza3			
											 in if (Up `elem` xs) then sprawdz' conf ((Pole i j) `go` Up) plansza4
											    else plansza4
									          
										   
sprawdz'' :: Configuration -> [[Int]] -> DlugoscBoku -> Bool
sprawdz'' conf xss n
-- porownuje konfiguracje z tablica dochodzenia pradu

