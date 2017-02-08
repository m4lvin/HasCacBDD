module Main where

import Data.HasCacBDD
import System.Exit

main :: IO ()
main = do
  putStrLn "\nRunning basic tests ..."
  if all snd tests
    then do
      putStrLn "All passed."
      exitSuccess
    else do
      putStrLn "FAILURES:"
      let failures = filter (not.snd) tests
      mapM_ (putStrLn . fst) failures
      putStrLn "All results:"
      mapM_ print tests
      exitFailure

tests :: [(String,Bool)]
tests =
  [ ("top == top", top == top)
  , ("top /= bot", top /= bot)
  , ("bot /= top", bot /= top)
  , ("bot == bot", bot == bot)
  , ("neg bot == top", neg bot == top)
  , ("neg bot /= bot", neg bot /= bot)
  , ("var 1 == var 1", var 1 == var 1)
  , ("var 5 /= var 7", var 5 /= var 7)
  , ("null (allSats bot)", null (allSats bot))
  , ("allSats top == [[]]", allSats top == [[]])
  , ("var 3 == con (var 3) top", var 3 == con (var 3) top)
  , ("var 4 /= con (var 3) top", var 4 /= con (var 3) top)
  , ("equ (var 1) (var 1) == top", equ (var 1) (var 1) == top)
  , ("exists 1 (neg $ var 1) == top", exists 1 (neg $ var 1) == top)
  , ("exists 1 (neg $ var 2) /= top", exists 1 (neg $ var 2) /= top)
  , ("gfp (\b -> con b (var 3)) == var 3", gfp (\b -> con b (var 3)) == var 3)
  , ("imp (conSet [var 1, var 0]) (var 1) == top", imp (conSet [var 1, var 0]) (var 1) == top)
  , ("imp (conSet [var 0, var 1]) (var 0) == top", imp (conSet [var 0, var 1]) (var 0) == top)
  , ("imp (con (var 0) (var 1)) (var 0) == top", imp (con (var 0) (var 1)) (var 0) == top)
  , ("show top == \"Top\"", show top == "Top")
  , ("show bot == \"Bot\"", show bot == "Bot")
  ]
