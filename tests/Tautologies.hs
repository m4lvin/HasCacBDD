module Main where

import Control.Applicative
import Data.HasCacBDD
import Text.Printf
import Test.QuickCheck

tests :: [(String, IO ())]
tests  = [ ("doubleNegation", quickCheck doubleNegation)
	  ,("excludedMiddle", quickCheck excludedMiddle)
	  ,("deMorganOne", quickCheck deMorganOne)
	  ,("deMorganTwo", quickCheck deMorganTwo)
	  ,("identityOne", quickCheck identityOne)
	  ,("quantifierDuality", quickCheck quantifierDuality) ]

main :: IO ()
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

doubleNegation :: Bdd -> Bool
doubleNegation b = neg (neg b) == b

excludedMiddle :: Bdd -> Bool
excludedMiddle b = b `dis` neg b == top

deMorganOne :: Bdd -> Bdd -> Bool
deMorganOne a b = neg (a `con` b) == (neg a `dis` neg b)

deMorganTwo :: Bdd -> Bdd -> Bool
deMorganTwo a b = neg (a `dis` b) == (neg a `con` neg b)

identityOne :: Bdd -> Bdd -> Bdd -> Bdd -> Bool
identityOne a b c d =
  conSet [a,b,c] `imp` d  ==  disSet [neg a, neg b, neg c] `dis` d

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
