module Main
where

import Data.HasCacBDD
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "Creating some BDDs:"
  putStrLn $ "top : " ++ (show $ top)
  putStrLn $ "bot : " ++ (show $ bot)
  putStrLn $ "var1: " ++ (show $ var 1)
  putStrLn $ "var1: " ++ (show $ var 1)
  putStrLn $ "var2: " ++ (show $ var 2)
  putStrLn $ "\nChecking some tautologies:"
  putStrLn $ show $ bot == bot
  putStrLn $ show $ top == top
  putStrLn $ show $ var 1 == var 1
  putStrLn $ show $ imp (var 1) (var 1) == top
  putStrLn $ show $ equ (var 1) (var 1) == top
  putStrLn $ show $ exists 1 (neg $ var 1) == top
  putStrLn $ show $ gfp (\b -> con b (var 3)) == (var 3)
  putStrLn $ "\nAnd some contradictions:"
  putStrLn $ show $ bot == top
  putStrLn $ show $ top == bot
  putStrLn $ show $ var 1 == top
  putStrLn $ show $ dis (var 1) (neg $ var 2) == top
  putStrLn $ show $ dis (var 1) (var 2) == top
  putStrLn $ show $ (var 1) == (var 2)
  putStrLn $ show $ forall 1 (var 1) == top
  putStrLn $ "\nLaws from de Morgan:"
  putStrLn $ show $ dis (neg $ var 1) (neg $ var 2) == (neg $ con (var 1) (var 2))
  putStrLn $ show $ con (neg $ var 1) (neg $ var 2) == (neg $ dis (var 1) (var 2))
  putStrLn $ "\nThe example from CacBDDs main.cpp: (!x[4] + !x[6]) * (!x[3] + !x[6]) * (!x[2] + !x[5])"
  let cacExample = conSet [ dis (neg (var 4)) (neg (var 6)) , (neg (var 3)) `dis` (neg (var 6)) , (neg (var 2)) `dis` (neg (var 5)) ]
  putStrLn $ show $ cacExample == top
  putStrLn "Good Bye."
