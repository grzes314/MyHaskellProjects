{-
  Grzegorz Łoś
  Program testujący. Przyjmuje, jako argumenty wywołania, plik z programem
  wejściowym, plik z programem
  zoptymalizowanym i wypisuje na standardowe wyjście porównanie ich działania.
-}


import Machine
import Interpreter
import MachineParser
import Misc

import Prelude hiding (compare)
import System.Environment   
import System.Directory  
import System.IO  
import System.Exit
import Data.List  
import System.Random
import Control.Monad

main :: IO ()
main = do
  testerName <- getProgName
  catch ( do {(fileInName:fileOptName:_) <- getArgs; return ()} )
        (\ _ -> (putStrLn $ "Usage: " ++ testerName ++ " program_in program_opt") >> exitFailure)
  (fileInName:fileOptName:_) <- getArgs
  progIn <- readFile fileInName
  progOpt <- readFile fileOptName
  let resIn = parseChar fileInName progIn
  case resIn of
  {  Left err -> putStrLn err >> exitFailure
  ;  Right char -> return ()
  }
  let resOpt = parseChar fileOptName progOpt
  case resOpt of
  {  Left err -> putStrLn err >> exitFailure
  ;  Right char -> return ()
  }
  putStrLn $ "Testing " ++ fileInName ++ " vs. " ++ fileOptName
  let (Right charIn) = resIn
  let (Right charOpt) = resOpt
  doStuff charIn charOpt fileInName fileOptName

{-
  Funkcja wykonująca zasadniczą część testowania. Przyjmuje maszynę wejściową,
  maszynę zoptymalizowaną, i wypisuje na standardowe wyjście porównanie
  ich działania. -}
doStuff :: Character -> Character -> String -> String -> IO ()
doStuff inn opt fileInName fileOptName = do
  let sizeIn = sizeCharacter inn
  let sizeOpt = sizeCharacter opt
  putStrLn $ "Size of " ++ fileInName ++ ": " ++ (show sizeIn)
  putStrLn $ "Size of " ++ fileOptName ++ ": " ++ (show sizeOpt)
  let vars = getVars inn
  let minMax = zip (map (getMin inn) vars) (map (getMax inn) vars)
  let minMax' = expand minMax
  --putStrLn $ "expanded: " ++ (show minMax')
  let statesRng = states inn
  results <- forM [1..10000] (\_ -> runTest inn opt vars minMax' statesRng)
  let (same, optNotWorse) = compare results
  putStrLn $ "The results of evaluation of machines where the same in " ++ (show same)
    ++ "% cases. Evaluation of optimized machine has given not bigger cost in "
    ++ (show optNotWorse) ++ "% cases."



-- Przyjmuje listę przedziałów i zwraca przedział dłuższy o ok 20%.
expand :: [(Integer, Integer)] -> [(Integer, Integer)]
expand [] = []
expand (h:t) = exp h : expand t where
  exp :: (Integer, Integer) -> (Integer, Integer)
  exp (a,b) = if (b < a)
                then (a,b)
                else if (b - a <= 10)
                  then (a-1,b+1)
                  else let d = floor $ 0.1 * toRational (b-a)
                        in (a-d,b+d)

{- Bierze listę par rezultatów ewaluacji na dwóch maszynach. Zakładamy, że
   pierwszy rezultat w parze pochodzi z maszyny wejściowej, a drugi z maszyny
   zoptymalizowanej. Zwraca parę Double'i. Pierwszy z nich oznacza w ilu
   procentach przypadków ewaluacja skończyła się tym samym wynikiem na obu
   maszynach (tzn. nowy stan i komunikat są takie same). Drugi oznacza w ilu
   procentach przypadków koszt ewaluacji na maszynie zoptymalizowanej był
   niegorszy. -}
compare :: [(Result, Result)] -> (Int, Int)
compare r = (same r, sndNotWorse r) where
  all = length r
  same r = let s = countSame r in 100 * s `div` all
  countSame [] = 0
  countSame (h:t) = let s = countSame t
                      in let ((ns1, utt1, _), (ns2, utt2, _)) = h
                         in if ns1 == ns2 && utt1 == utt2
                                then s+1
                                else s
  sndNotWorse r = let s = countNotWorse r in 100 * s `div` all
  countNotWorse [] = 0
  countNotWorse (h:t) = let c = countNotWorse t
                      in let ((_, _, cost1), (_, _, cost2)) = h
                         in if cost2 <= cost1
                                then c+1
                                else c

{- Uruchamia pojedynczy test. Przyjmuje dwie maszyny (pierwsza jest wejściowa, 
   a druga zoptymalizowana), listę nazw zmiennych, listę przedziałów wartości
   jakie mogą przyjmować zmienne, listę możliwych stanów początkowych. Następnie
   losowane są dane - wartości zmiennych i stan początkowy. Funkcja zwraca
   akcje IO, której wynikiem jest para rezultatów ewaluacji tych danych na
   obu maszynach -}
runTest :: Character -> Character -> [String] -> [(Integer, Integer)] -> [Integer] -> IO (Result, Result)
runTest inn opt vars minMax statesRng = do
  st <- losuj statesRng
  gen <- newStdGen
  let dict = makeDict vars minMax gen
  --putStrLn $ "state: " ++ (show st) ++ " dict: " ++ (show dict)
  let resIn = nextState inn st dict
  let resOpt = nextState opt st dict
  --putStrLn $ "Results - in: " ++ (show resIn) ++ " opt: " ++ (show resOpt)
  return (resIn, resOpt) where
    losuj statesRng = do
      let l = length statesRng
      gen <- newStdGen
      let (r, _) = randomR (0,l-1) gen
      return $ statesRng !! r

{- Bierze listę nazw zmiennych, listę przedziałów wartości jakie mogą
   przyjmować zmienne oraz generator losowości. Generuje słownik wartości
   zmiennych z wartościami wylosowanymi z przedziałów. -}
makeDict :: [String] -> [(Integer, Integer)] -> StdGen -> [(String, Integer)]
makeDict vars minMax gen = zip vars (makeRandomList minMax gen) where
   makeRandomList:: [(Integer, Integer)] -> StdGen -> [Integer]
   makeRandomList [] gen = []
   makeRandomList (h:t) gen = let (r, gen2) = randomR h gen
                              in r : makeRandomList t gen2



