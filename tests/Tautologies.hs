module Main where

import Data.HasCacBDD
import Data.Maybe
import Text.Printf
import Test.QuickCheck
import Test.QuickCheck.Test
import Control.Monad
import System.Exit

main :: IO ()
main  = do
  results <- mapM (\(s,a) -> printf "%-25s: " s >> a) tests
  unless (all isSuccess results) exitFailure
  showInfo

tests :: [(String, IO Result)]
tests  =
  [("selfEqual",      quickCheckResult (\b -> equ b b == top))
  ,("doubleNegation", quickCheckResult (\b -> neg (neg b) == b))
  ,("selfImp",        quickCheckResult (\b -> imp b b == top))
  ,("selfEquiv",      quickCheckResult (\b -> equ b b == top))
  ,("excludedMiddle", quickCheckResult (\b -> b `dis` neg b == top))
  ,("deMorganOne",    quickCheckResult (\a b -> neg (a `con` b) == (neg a `dis` neg b)))
  ,("deMorganTwo",    quickCheckResult (\a b -> neg (a `dis` b) == (neg a `con` neg b)))
  ,("identityOne",    quickCheckResult (\a b c d  -> conSet [a,b,c] `imp` d  ==  disSet [neg a, neg b, neg c] `dis` d))
  ,("conElim",        quickCheckResult (\a b -> imp (con a b) a == top))
  ,("conElim3",       quickCheckResult (\a b c -> imp (conSet [a, b, c]) a == top))
  ,("negNotEqual",    quickCheckResult (\b -> neg b /= b))
  ,("quantifiers",    quickCheckResult (forAll (elements [0..maximumvar]) quantifierDuality))
  ,("allSats",        quickCheckResult (\b -> all (\s -> restrictSet b s == top) (allSats b)))
  ,("anySat",         quickCheckResult (\b -> b==bot || restrictSet b (fromMaybe [] $ anySat b) == top))
  ,("ifthenelse",     quickCheckResult (\a b c -> ifthenelse a b c == neg (dis (con a (neg b)) (con (neg a) (neg c)))))
  ,("ravel-unravel",  quickCheckResult (\b -> b == ravel (unravel b)))
  ,("firstVarOf",     quickCheckResult (\b -> b `elem` [bot,top] || Just (head (allVarsOf b)) == firstVarOf b))
  ,("maxVarOf",       quickCheckResult (\b -> b `elem` [bot,top] || Just (last (allVarsOf b)) == maxVarOf   b))
  ,("thenOf",         quickCheckResult (\b -> b `elem` [bot,top] || thenOf b == restrict b (fromMaybe 1 $ firstVarOf b, True )))
  ,("elseOf",         quickCheckResult (\b -> b `elem` [bot,top] || elseOf b == restrict b (fromMaybe 1 $ firstVarOf b, False)))
  -- TODO: gfp
  ]

quantifierDuality :: Int -> Bdd -> Bool
quantifierDuality n b = forall n b == neg (exists n (neg b))
