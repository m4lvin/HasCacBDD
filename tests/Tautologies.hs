module Main where

import Control.Monad
import Data.HasCacBDD
import Data.List
import Data.Maybe
import Data.Tuple
import Text.Printf
import Test.QuickCheck
import Test.QuickCheck.Test
import System.Exit

main :: IO ()
main  = do
  results <- mapM (\(s,a) -> printf "%-25s: " s >> a) tests
  unless (all isSuccess results) exitFailure
  showInfo

tests :: [(String, IO Result)]
tests  =
  [("selfEqual",      quickCheckResult (\b -> (b::Bdd) == b))
  ,("idSymmetry",     quickCheckResult (\a b -> ((a::Bdd) == (b::Bdd)) == (b == a)))
  ,("singleNegation", quickCheckResult (\b -> neg b /= b))
  ,("doubleNegation", quickCheckResult (\b -> neg (neg b) == b))
  ,("selfImp",        quickCheckResult (\b -> imp b b == top))
  ,("selfEqu",        quickCheckResult (\b -> equ b b == top))
  ,("selfXor",        quickCheckResult (\b -> xor b b == bot))
  ,("excludedMiddle", quickCheckResult (\b -> b `dis` neg b == top))
  ,("deMorganOne",    quickCheckResult (\a b -> neg (a `con` b) == (neg a `dis` neg b)))
  ,("deMorganTwo",    quickCheckResult (\a b -> neg (a `dis` b) == (neg a `con` neg b)))
  ,("identityOne",    quickCheckResult (\as b -> conSet as `imp` b  ==  disSet (map neg as) `dis` b))
  ,("conElim",        quickCheckResult (\a b -> imp (con a b) a == top))
  ,("conElim3",       quickCheckResult (\a b c -> imp (conSet [a, b, c]) a == top))
  ,("negNotEqual",    quickCheckResult (\b -> neg b /= b))
  ,("quantifDuality", quickCheckResult (forAll (elements [0..maximumvar]) (\n b -> forall n b == neg (exists n (neg b)))))
  ,("allSats",        quickCheckResult (\b -> all (\s -> restrictSet b s == top) (allSats b)))
  ,("anySat",         quickCheckResult (\b -> if b==bot then isNothing (anySat b) else restrictSet b (fromJust $ anySat b) == top))
  ,("ifthenelse",     quickCheckResult (\a b c -> ifthenelse a b c == neg (dis (con a (neg b)) (con (neg a) (neg c)))))
  ,("ravel-unravel",  quickCheckResult (\b -> b == ravel (unravel b)))
  ,("firstVarOf",     quickCheckResult (\b -> if b `elem` [bot,top] then isNothing (firstVarOf b) else Just (head (allVarsOf b)) == firstVarOf b))
  ,("maxVarOf",       quickCheckResult (\b -> if b `elem` [bot,top] then isNothing (maxVarOf b) else Just (last (allVarsOf b)) == maxVarOf   b))
  ,("thenOf",         quickCheckResult (\b -> if b `elem` [bot,top] then thenOf b == b else thenOf b == restrict b (fromJust $ firstVarOf b, True )))
  ,("elseOf",         quickCheckResult (\b -> if b `elem` [bot,top] then elseOf b == b else elseOf b == restrict b (fromJust $ firstVarOf b, False)))
  ,("deMorganOneSet", quickCheckResult (\as -> neg (conSet as) ==  disSet (map neg as)))
  ,("deMorganTwoSet", quickCheckResult (\as -> neg (disSet as) ==  conSet (map neg as)))
  ,("xorSetCommute",  quickCheckResult (\a as -> xorSet (a:as) == xor (xorSet as) a))
  ,("gfpCon",         quickCheckResult (\b -> gfp (`con` b) == b))
  ,("sizeNeg",        quickCheckResult (\b -> sizeOf b == sizeOf (neg b)))
  ,("restrictLaw",    quickCheckResult (\a b -> b `imp` equ (restrictLaw a b) a == top))
  ,("allSatsWith",    quickCheckResult (\b -> all (\s -> restrictSet b s == top) (allSatsWith (allVarsOf b) b)))
  ,("anySatWith",     quickCheckResult (\b -> let vs = allVarsOf b in if b==bot then isNothing (anySatWith vs b) else restrictSet b (fromJust $ anySatWith vs b) == top))
  ,("satCountWith",   quickCheckResult (\b -> let vs = allVarsOf b in length (allSatsWith vs b) == satCountWith vs b))
  ,("subOf",          quickCheckResult (\b -> all (`elem` subOf b) (subOf $ thenOf b)))
  , let relabelTest b c = relabel gnippam (relabel mapping b) == b where
          vs = reverse $ nub (allVarsOf b ++ allVarsOf c)
          mapping = zip vs (map (+100) vs)
          gnippam = map swap mapping
    in ("relabel",    quickCheckResult relabelTest)
  , ("show",          quickCheckResult (\a b -> (show (unravel a) == show (unravel b)) == (a == (b::Bdd))))
  , ("showList",      quickCheckResult (\a b -> (showList [unravel a] "" == showList [unravel b] "") == (a == (b::Bdd))))
  ]
