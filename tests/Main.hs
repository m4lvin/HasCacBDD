module Main where

import Data.HasCacBDD
import Data.List (nub)
import Data.Maybe (fromJust,isNothing)
import Data.Tuple (swap)
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main  = hspec $ do
  describe "Examples" $ do
    describe "Creating BDDs" $ do
      it "top == Top" $ show top `shouldBe` "Top"
      it "" $ show bot `shouldBe` "Bot"
      it "" $ show (var 1) `shouldBe` "Var 1 Top Bot"
      it "" $ show (var 1) `shouldBe` "Var 1 Top Bot"
      it "" $ show (var 2) `shouldBe` "Var 2 Top Bot"
    describe "Some tautologies" $ do
      it "bot == bot" $ bot `shouldBe` bot
      it "top == top" $ top `shouldBe` top
      it "var 1 == var 1" $ var 1 `shouldBe` var 1
      it "imp (var 1) (var 1) == top" $ imp (var 1) (var 1) `shouldBe` top
      it "equ (var 1) (var 1) == top" $ equ (var 1) (var 1) `shouldBe` top
      it "exists 1 (neg $ var 1) == top" $ exists 1 (neg $ var 1) `shouldBe` top
      it "gfp (\b -> con b (var 3)) == var 3" $ gfp (\b -> con b (var 3)) `shouldBe` var 3
      it "imp (conSet [var 1, var 0]) (var 1) == top" $ imp (conSet [var 1, var 0]) (var 1) `shouldBe` top
      it "imp (conSet [var 0, var 1]) (var 0) == top" $ imp (conSet [var 0, var 1]) (var 0) `shouldBe` top
      it "imp (con (var 0) (var 1)) (var 0) == top" $ imp (con (var 0) (var 1)) (var 0) `shouldBe` top
    describe "Some non-tautologies" $ do
      it "bot /= top" $ bot `shouldNotBe` top
      it "top /= bot" $ top `shouldNotBe` bot
      it "var 1 /= top" $ var 1 `shouldNotBe` top
      it "dis (var 1) (neg $ var 2) /= top" $ dis (var 1) (neg $ var 2) `shouldNotBe` top
      it "dis (var 1) (var 2) /= top" $ dis (var 1) (var 2) `shouldNotBe` top
      it "var 1 /= var 2" $ var 1 `shouldNotBe` var 2
      it "forall 1 (var 1) /= top" $ forall 1 (var 1) `shouldNotBe` top
    describe "Laws from de Morgan:" $ do
      it "" $ dis (neg $ var 1) (neg $ var 2) == neg (con (var 1) (var 2))
      it "" $ con (neg $ var 1) (neg $ var 2) == neg (dis (var 1) (var 2))
    it "The example from CacBDDs main.cpp (~x[4] + ~x[6]) * (~x[3] + ~x[6]) * (~x[2] + ~x[5]) /= top" $
      conSet [ dis (neg (var 4)) (neg (var 6)) , neg (var 3) `dis` neg (var 6), neg (var 2) `dis` neg (var 5) ] `shouldNotBe` top
    it "showInfo works" $
      showInfo `shouldReturn` ()
  describe "Basics" $ do
    it "top == top" $ top `shouldBe` top
    it "top /= bot" $ top `shouldNotBe` bot
    it "bo /= top" $ bot `shouldNotBe` top
    it "bot == bot" $ bot `shouldBe` bot
    it "neg bot == top" $ neg bot `shouldBe` top
    it "neg bot /= bot" $ neg bot `shouldNotBe` bot
    it "var 1 == var 1" $ var 1 `shouldBe` var 1
    it "var 5 == var 7" $ var 5 `shouldNotBe` var 7
    it "null (allSats bot)" $ null (allSats bot)
    it "allSats top `==` [[]]" $ allSats top `shouldBe` [[]]
    it "var 3 == con (var 3) top" $ var 3 `shouldBe` con (var 3) top
    it "var 4 /= con (var 3) top" $ var 4 `shouldNotBe` con (var 3) top
    it "equ (var 1) (var 1) == top" $ equ (var 1) (var 1) `shouldBe` top
    it "exists 1 (neg $ var 1) == top" $ exists 1 (neg $ var 1) `shouldBe` top
    it "exists 1 (neg $ var 2) /= top" $ exists 1 (neg $ var 2) `shouldNotBe` top
    it "gfp (\b -> con b (var 3)) == var 3" $ gfp (\b -> con b (var 3)) `shouldBe` var 3
    it "imp (conSet [var 1,var 0]) (var 1) == top" $ imp (conSet [var 1,var 0]) (var 1) `shouldBe` top
    it "imp (conSet [var 0,var 1]) (var 0) == top" $ imp (conSet [var 0,var 1]) (var 0) `shouldBe` top
    it "imp (con (var 0) (var 1)) (var 0) == top" $ imp (con (var 0) (var 1)) (var 0) `shouldBe` top
    it "show top == \"Top\"" $ show top `shouldBe` "Top"
    it "show bot == \"Bot\"" $ show bot `shouldBe` "Bot"
  describe "QuickCheck Properties" $ do
    prop "selfEqual"      (\b -> (b::Bdd) == b)
    prop "showReadEqual"  (\b -> read (show b) == (b::Bdd))
    prop "showReadTreeEq" (\b -> (ravel . read . show . unravel $ b) == b)
    prop "idSymmetry"     (\a b -> ((a::Bdd) == (b::Bdd)) == (b == a))
    prop "singleNegation" (\b -> neg b /= b)
    prop "doubleNegation" (\b -> neg (neg b) == b)
    prop "selfImp"        (\b -> imp b b == top)
    prop "selfEqu"        (\b -> equ b b == top)
    prop "selfXor"        (\b -> xor b b == bot)
    prop "excludedMiddle" (\b -> b `dis` neg b == top)
    prop "deMorganOne"    (\a b -> neg (a `con` b) == (neg a `dis` neg b))
    prop "deMorganTwo"    (\a b -> neg (a `dis` b) == (neg a `con` neg b))
    prop "identityOne"    (\as b -> conSet as `imp` b  ==  disSet (map neg as) `dis` b)
    prop "conElim"        (\a b -> imp (con a b) a == top)
    prop "conElim3"       (\a b c -> imp (conSet [a, b, c]) a == top)
    prop "negNotEqual"    (\b -> neg b /= b)
    prop "quantifDuality" (forAll (elements [0..maximumvar]) (\n b -> forall n b == neg (exists n (neg b))))
    prop "allSats"        (\b -> all (\s -> restrictSet b s == top) (allSats b))
    prop "anySat"         (\b -> if b==bot then isNothing (anySat b) else restrictSet b (fromJust $ anySat b) == top)
    prop "ifthenelse"     (\a b c -> ifthenelse a b c == neg (dis (con a (neg b)) (con (neg a) (neg c))))
    prop "ravel-unravel"  (\b -> b == ravel (unravel b))
    prop "firstVarOf"     (\b -> if b `elem` [bot,top] then isNothing (firstVarOf b) else Just (head (allVarsOfSorted b)) == firstVarOf b)
    prop "maxVarOf"       (\b -> if b `elem` [bot,top] then isNothing (maxVarOf b) else Just (last (allVarsOfSorted b)) == maxVarOf   b)
    prop "thenOf"         (\b -> if b `elem` [bot,top] then thenOf b == b else thenOf b == restrict b (fromJust $ firstVarOf b, True ))
    prop "elseOf"         (\b -> if b `elem` [bot,top] then elseOf b == b else elseOf b == restrict b (fromJust $ firstVarOf b, False))
    prop "deMorganOneSet" (\as -> neg (conSet as) ==  disSet (map neg as))
    prop "deMorganTwoSet" (\as -> neg (disSet as) ==  conSet (map neg as))
    prop "conSetCommute"  (\a as -> conSet (a:as) == con (conSet as) a)
    prop "disSetCommute"  (\a as -> disSet (a:as) == dis (disSet as) a)
    prop "xor-disSet"     ( (\as -> xorSet as `imp` disSet as == top) . (take 23) )
    prop "xorSetNeg3"     (\a b c -> xorSet [a,b,c] == xorSet [neg a, b, neg c])
    prop "xorSetCommute3" (\a b c -> xorSet [a,b,c] == xor (xorSet [a,b]) c)
    prop "xorSetCommute4" (\a b c d -> xorSet [a,b,c,d] == xor (xorSet [b,c,a]) d)
    prop "gfpCon"         (\b -> gfp (`con` b) == b)
    prop "sizeNeg"        (\b -> sizeOf b == sizeOf (neg b))
    prop "restrictLaw"    (\a b -> b `imp` equ (restrictLaw a b) a == top)
    prop "evaluate"       (\b -> all (\s -> evaluate b s == Just True) (allSatsWith (allVarsOf b) b))
    prop "evaluateFun"    (\b -> all (\s -> evaluateFun b (\n -> fromJust $ lookup n s)) (allSats b))
    prop "evaluateFun F"  (\b -> all (\s -> not (evaluateFun bot (\n -> fromJust $ lookup n s))) (allSats b))
    prop "allSatsWith"    (\b -> all (\s -> restrictSet b s == top) (allSatsWith (allVarsOf b) b))
    prop "anySatWith"     (\b -> let vs = allVarsOf b in if b==bot then isNothing (anySatWith vs b) else restrictSet b (fromJust $ anySatWith vs b) == top)
    prop "satCountWith"   (\b -> let vs = allVarsOf b in length (allSatsWith vs b) == satCountWith vs b)
    prop "subsOf"         (\b -> all (`elem` subsOf b) (subsOf $ thenOf b))
    prop "relabel"        (\b c -> let
                                      vs = reverse $ nub (allVarsOf b ++ allVarsOf c)
                                      mapping = zip vs (map (+100) vs)
                                      gnippam = map swap mapping
                                   in
                                      relabel gnippam (relabel mapping b) == b)
    prop "relabelFun"    (\a -> relabelFun (\x -> x-7) (relabelFun (+7) a) == a)
    prop "substit"       (\b c -> substit 5 b c == ifthenelse b (restrict c (5,True)) (restrict c (5,False)))
    prop "show"          (\a b -> (show a == show b) == (a == (b::Bdd)))
    prop "read"          (\b -> read (show b) == (b :: Bdd))
    prop "showList"      (\a b -> (showList [unravel a] "" == showList [unravel b] "") == (a == (b::Bdd)))
  describe "QuickCheck Expected Failures" $ do
    prop "wrong deMorganOne" $
      expectFailure (\a b -> neg (a `con` b) === (neg a `con` neg b))
    prop "wrong deMorganTwo" $
      expectFailure (\a b -> neg (a `dis` b) === (neg a `dis` neg b))
    modifyMaxSuccess (* 1000) $ prop "folding substit is not the same as substitSimul" $
      expectFailure (\b1 b2 c -> foldl (flip $ uncurry substit) c [(1,b1),(2,b2)] === substitSimul [(1,b1),(2,b2)] c)
