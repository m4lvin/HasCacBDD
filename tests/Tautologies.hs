module Main where

import Control.Applicative
import Data.HasCacBDD
import Text.Printf
import Test.QuickCheck
import Test.QuickCheck.Test
import Control.Monad
import System.Exit

main :: IO ()
main  = do
  results <- mapM (\(s,a) -> printf "%-25s: " s >> a) tests
  unless (all isSuccess results) exitFailure

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
  ,("quantifiers",    quickCheckResult quantifierDuality) ]
  -- TODO: gfp

maximumvar :: Int
maximumvar = 100

data VarInt = VarInt Int deriving Show

instance Arbitrary VarInt where
  arbitrary = VarInt <$> elements [0..maximumvar]

quantifierDuality :: VarInt -> Bdd -> Bool
quantifierDuality vi b = forall n b == neg (exists n (neg b)) where (VarInt n) = vi

instance Arbitrary Bdd where
  arbitrary = sized randombdd

randombdd ::  Int -> Gen Bdd
randombdd sz = bdd' sz' where
  sz' = min maximumvar sz
  bdd' 0 = var <$> choose (0, sz')
  bdd' n = oneof [  pure top
		  , pure bot
		  , var <$> choose (0, sz')
		  , neg <$> st
		  , con <$> st <*> st
		  , dis <$> st <*> st
		  , imp <$> st <*> st
		  , equ <$> st <*> st
		  , xor <$> st <*> st
		  , exists <$> randomvar <*> st
		  , forall <$> randomvar <*> st
		  ]
    where
      st = bdd' (n `div` 2)
      randomvar = elements [0..maximumvar]
