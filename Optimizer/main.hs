{-
  Grzegorz Łoś
  Program główny. Przyjmuje plik z maszyną, czas, w którym mają zakończyć się
  obliczenia i zapisuje wynik do pliku o takiej samej nazwie, ale z rozszerzeniem
  ".opt".
-}


import Machine
import MachineParser
import Optimizer

import System.Environment   
import System.Directory  
import System.IO  
import System.Exit
import Data.List  

main :: IO ()
main = do
  progName <- getProgName
  catch ( do {(fileName:timeStr:_) <- getArgs; return ()} )
        (\ _ -> (putStrLn $ "Usage: " ++ progName ++ " program_file maxTime") >> exitFailure)
  (fileName:timeStr:_) <- getArgs
  prog <- readFile fileName
  let time = read timeStr :: Int
  let res = parseChar fileName prog
  case res of
  {  Left err -> putStrLn err
  ;  Right char -> doStuff char fileName time
  }
  
  
doStuff :: Character -> String -> Int -> IO ()
doStuff char fileName time = do
         let opt = optimize char time
         writeFile (fileName ++ ".opt") (show opt)

