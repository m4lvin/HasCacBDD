module Main where

import Data.HasCacBDD.Safe

-- This imitates the `c/main.c` example.
main :: IO ()
main = do
  mgr <- xBddManager_new 1048576
  putStrLn $ "Haskell has mgr = " ++ show mgr
  b <- bdd_new 8
  v <- xBddManager_BddOne b mgr
  putStrLn $ "Haskell has BDD at " ++ show v
