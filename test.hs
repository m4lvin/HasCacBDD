module Main
where

import System.IO
import Data.HasCacBDD

main :: IO ()
main = do
  putStrLn $ " bot == bot \t\t\t\t" ++( show $ bot == bot )
  putStrLn $ " bot == top \t\t\t\t" ++( show $ bot == top )
  putStrLn $ " top == top \t\t\t\t" ++( show $ top == top )
  putStrLn $ " var 1 == var 1 \t\t\t" ++( show $ var 1 == var 1 )
  putStrLn $ " var 1 == var 2 \t\t\t" ++( show $ var 1 == var 2 )
  putStrLn $ " var 1 == top \t\t\t\t" ++( show $ var 1 == top )
  putStrLn $ " dis (var 1) (neg $ var 1) == top \t" ++( show $ dis (var 1) (neg $ var 1) == top )
  putStrLn $ " dis (var 1) (neg $ var 2) == top \t" ++( show $ dis (var 1) (neg $ var 2) == top )
  putStrLn $ " dis (var 1) (var 2) == top \t\t" ++( show $ dis (var 1) (var 2) == top )
  putStrLn $ " imp (var 1) (var 1) == top \t\t" ++( show $ imp (var 1) (var 1) == top )
  putStrLn $ " equ (var 1) (var 1) == top \t\t" ++( show $ equ (var 1) (var 1) == top )
  -- The example from CacBDD's main.cpp: (!x[4] + !x[6]) * (!x[3] + !x[6]) * (!x[2] + !x[5]);
  let cacExample = conSet [ dis (neg (var 4)) (neg (var 6)) , (neg (var 3)) `dis` (neg (var 6)) , (neg (var 2)) `dis` (neg (var 5)) ]
  putStrLn $ " (!x4+!x6)*(!x3+!x6)*(!x2+!x5) == top \t" ++ ( show $ cacExample == top )
  putStrLn "Good Bye."
