module Main where

import Data.HasCacBDD
import System.Exit

main :: IO ()
main = do
  putStrLn "\nRunning CacBDD example in C++ ..."
  runexample
  putStrLn "\nRunning CacBDD example in Haskell ..."
  let cacExample = conSet [ dis (neg (var 4)) (neg (var 6)) , neg (var 3) `dis` neg (var 6), neg (var 2) `dis` neg (var 5) ]
  print cacExample
  putStr "Equal to top? "
  print $ cacExample == top
  putStr "Equal top to? "
  print $ top == cacExample
  putStr "Equal to bot? "
  print $ cacExample == bot
  putStr "Equal bot to? "
  print $ bot == cacExample
  putStrLn "\nRunning basic tests ..."
  if all snd tests
    then do
      putStrLn "All passed."
      exitSuccess
    else do
      putStrLn "FAILURES:"
      let failures = filter (not.snd) tests
      mapM_ (putStrLn . fst) failures
      exitFailure

tests :: [(String,Bool)]
tests =
  [ ("bot == bot", bot == bot)
  , ("top == top", top == top)
  , ("neg bot == top", neg bot == top)
  , ("neg bot /= bot", neg bot /= bot)
  , ("bot /= top", bot /= top)
  , ("var 1 == var 1", var 1 == var 1)
  , ("var 5 /= var 7", var 5 /= var 7)
  , ("var 3 == con (var 3) top", var 3 == con (var 3) top)
  , ("var 4 /= con (var 3) top", var 4 /= con (var 3) top)
  , ("equ (var 1) (var 1) == top", equ (var 1) (var 1) == top)
  , ("exists 1 (neg $ var 1) == top", exists 1 (neg $ var 1) == top)
  , ("exists 1 (neg $ var 2) /= top", exists 1 (neg $ var 2) /= top)
  , ("gfp (\b -> con b (var 3)) == var 3", gfp (\b -> con b (var 3)) == var 3)
  , ("imp (conSet [var 1, var 0]) (var 1) == top", imp (conSet [var 1, var 0]) (var 1) == top)
  , ("imp (conSet [var 0, var 1]) (var 0) == top", imp (conSet [var 0, var 1]) (var 0) == top)
  , ("imp (con (var 0) (var 1)) (var 0) == top", imp (con (var 0) (var 1)) (var 0) == top)
  ]
