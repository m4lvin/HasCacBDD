module Main
where

import Data.HasCacBDD
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Creating some BDDs:"
  putStrLn $ "top : " ++ show top
  putStrLn $ "bot : " ++ show bot
  putStrLn $ "var1: " ++ show (var 1)
  putStrLn $ "var1: " ++ show (var 1)
  putStrLn $ "var2: " ++ show (var 2)
  putStrLn "\nChecking some tautologies:"
  print $ bot == bot
  print $ top == top
  print $ var 1 == var 1
  print $ imp (var 1) (var 1) == top
  print $ equ (var 1) (var 1) == top
  print $ exists 1 (neg $ var 1) == top
  print $ gfp (\b -> con b (var 3)) == var 3
  print $ imp (conSet [var 1, var 0]) (var 1) == top
  print $ imp (conSet [var 0, var 1]) (var 0) == top
  print $ imp (con (var 0) (var 1)) (var 0) == top
  putStrLn "\nAnd some contradictions:"
  print $ bot == top
  print $ top == bot
  print $ var 1 == top
  print $ dis (var 1) (neg $ var 2) == top
  print $ dis (var 1) (var 2) == top
  print $ var 1 == var 2
  print $ forall 1 (var 1) == top
  putStrLn "\nLaws from de Morgan:"
  print $ dis (neg $ var 1) (neg $ var 2) == neg (con (var 1) (var 2))
  print $ con (neg $ var 1) (neg $ var 2) == neg (dis (var 1) (var 2))
  putStrLn "\nThe example from CacBDDs main.cpp: (!x[4] + !x[6]) * (!x[3] + !x[6]) * (!x[2] + !x[5])"
  let cacExample = conSet [ dis (neg (var 4)) (neg (var 6)) , neg (var 3) `dis` neg (var 6), neg (var 2) `dis` neg (var 5) ]
  print $ cacExample
  print $ cacExample == top
  putStrLn "Good Bye."
