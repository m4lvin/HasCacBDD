-- | Import CacBDD functions and make them available under nice names.

{-# LANGUAGE ForeignFunctionInterface #-}
module Data.HasCacBDD (
  -- * Types
  Bdd, Assignment,
  -- * Creation of new BDDs
  top, bot, var,
  -- * Combination and Manipulation of BDDs
  neg, con, dis, imp, equ, xor, conSet, disSet, xorSet,
  exists, forall, forallSet, existsSet,
  restrict, restrictSet,
  ifthenelse, gfp, relabel,
  -- * Get satisfying assignments
  allSats, allSatsWith, satCountWith, anySat, anySatWith,
  -- * Show and convert to trees
  BddTree(..), unravel, ravel, firstVarOf
) where

import Data.Word
import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe
import Data.List

-- | This datatype for Binary Decision diagrams has no internal structure
-- because from our perspective BDDs are just pointers.
newtype Bdd = Bdd (Ptr Bdd)

newtype XBddManager = XBddManager (Ptr XBddManager) deriving (Show)

foreign import ccall unsafe "BDDNodeC.h BDD_new" bdd_new :: Word -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h XBDDManager_new" xBddManager_new :: CInt -> IO XBddManager
foreign import ccall unsafe "BDDNodeC.h XBDDManager_BddOne"  xBddManager_BddOne  :: Bdd -> XBddManager -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h XBDDManager_BddZero" xBddManager_BddZero :: Bdd -> XBddManager -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h XBDDManager_BddVar"  xBddManager_BddVar  :: Bdd -> XBddManager -> CInt -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h XBDDManager_Ite"     xBddManager_Ite     :: Bdd -> XBddManager -> Bdd -> Bdd -> Bdd -> IO Bdd

foreign import ccall unsafe "BDDNodeC.h BDD_Operator_Equal"  bdd_Operator_Equal  :: Bdd -> Bdd -> IO Bool
foreign import ccall unsafe "BDDNodeC.h BDD_Operator_Not"    bdd_Operator_Not    :: Bdd -> Bdd -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h BDD_Operator_Or"     bdd_Operator_Or     :: Bdd -> Bdd -> Bdd -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h BDD_Operator_And"    bdd_Operator_And    :: Bdd -> Bdd -> Bdd -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h BDD_Operator_Xor"    bdd_Operator_Xor    :: Bdd -> Bdd -> Bdd -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h BDD_Operator_LessEqual" bdd_Operator_LessEqual :: Bdd -> Bdd -> Bdd -> IO Bdd -- only use this with a patched CacBDD, see CacBDD-Manager.cpp.patch.
foreign import ccall unsafe "BDDNodeC.h BDD_Exist"           bdd_Exist           :: Bdd -> Bdd -> Bdd -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h BDD_Universal"       bdd_Universal       :: Bdd -> Bdd -> Bdd -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h BDD_Restrict"        bdd_Restrict        :: Bdd -> Bdd -> Bdd -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h BDD_Variable"        bdd_Variable        :: Bdd -> IO CInt
foreign import ccall unsafe "BDDNodeC.h BDD_Then"            bdd_Then            :: Bdd -> Bdd -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h BDD_Else"            bdd_Else            :: Bdd -> Bdd -> IO Bdd

manager :: XBddManager
manager = unsafePerformIO (xBddManager_new 1048576) -- fix the number of variables
{-# NOINLINE manager #-}

-- | Restrict a given variable to a given value
restrict :: Bdd -> (Int,Bool) -> Bdd
restrict b (n,bit) = unsafePerformIO $ bdd_Restrict (unsafePerformIO (bdd_new 8)) b res where
  res = if bit then (var n) else neg (var n)
{-# NOINLINE restrict #-}

-- | Restrict several variables to given values
restrictSet :: Bdd -> [(Int,Bool)] -> Bdd
restrictSet b bits = unsafePerformIO $ bdd_Restrict (unsafePerformIO (bdd_new 8)) b res where
  res = conSet $ map (\(n,bit) -> if bit then (var n) else neg (var n)) bits
{-# NOINLINE restrictSet #-}

-- | Existential Quantification
exists :: Int -> Bdd -> Bdd
exists n b = unsafePerformIO $ bdd_Exist (unsafePerformIO (bdd_new 8)) b (var n)
{-# NOINLINE exists #-}

-- | Universal Quantification
forall :: Int -> Bdd -> Bdd
forall n b = unsafePerformIO $ bdd_Universal (unsafePerformIO (bdd_new 8)) b (var n)
{-# NOINLINE forall #-}

-- | Big Existential Quantification
existsSet :: [Int] -> Bdd -> Bdd
existsSet ns b = foldl (flip exists) b ns

-- | Big Universal Quantification
forallSet :: [Int] -> Bdd -> Bdd
forallSet ns b = foldl (flip forall) b ns

-- | True constant
top :: Bdd
top = unsafePerformIO (xBddManager_BddOne (unsafePerformIO (bdd_new 8)) manager)
{-# NOINLINE top #-}

-- | False constant
bot :: Bdd
bot = unsafePerformIO (xBddManager_BddZero (unsafePerformIO (bdd_new 8)) manager)
{-# NOINLINE bot #-}

-- | Variable, indexed by any integer from 0 to 1.000.000
var :: Int -> Bdd
var n = unsafePerformIO (xBddManager_BddVar (unsafePerformIO (bdd_new 8)) manager (fromIntegral (n+1)))
{-# NOINLINE var #-}

-- | If ... then ... else ...
ifthenelse :: Bdd -> Bdd -> Bdd -> Bdd
ifthenelse test yes no = unsafePerformIO (xBddManager_Ite (unsafePerformIO (bdd_new 8)) manager test yes no)
{-# NOINLINE ifthenelse #-}

instance Eq Bdd where
  b1 == b2 = same b1 b2

same :: Bdd -> Bdd -> Bool
same b1 b2 = unsafePerformIO (bdd_Operator_Equal b1 b2)
{-# NOINLINE same #-}

-- | Negation
neg :: Bdd -> Bdd
neg b = unsafePerformIO (bdd_Operator_Not (unsafePerformIO (bdd_new 8)) b)
{-# NOINLINE neg #-}

-- | Equivalence aka Biimplication
equ :: Bdd -> Bdd -> Bdd
equ b1 b2 = con (imp b1 b2) (imp b2 b1) -- ugly...
{-# NOINLINE equ #-}

-- | Implication
imp :: Bdd -> Bdd -> Bdd
imp b1 b2 = unsafePerformIO (bdd_Operator_LessEqual (unsafePerformIO (bdd_new 8)) b1 b2)
{-# NOINLINE imp #-}

-- | Conjunction
con :: Bdd -> Bdd -> Bdd
con b1 b2 = unsafePerformIO (bdd_Operator_And (unsafePerformIO (bdd_new 8)) b1 b2)
{-# NOINLINE con #-}

-- | Disjunction
dis :: Bdd -> Bdd -> Bdd
dis b1 b2 = unsafePerformIO (bdd_Operator_Or (unsafePerformIO (bdd_new 8)) b1 b2)
{-# NOINLINE dis #-}

-- | Exclusive Or
xor :: Bdd -> Bdd -> Bdd
xor b1 b2 = unsafePerformIO (bdd_Operator_Xor (unsafePerformIO (bdd_new 8)) b1 b2)
{-# NOINLINE xor #-}

-- | Big Conjunction
conSet :: [Bdd] -> Bdd
conSet [] = top
conSet (b:bs) =
  if elem bot (b:bs)
    then bot
    else foldl con b bs
{-# NOINLINE conSet #-}

-- | Big Disjunction
disSet :: [Bdd] -> Bdd
disSet [] = bot
disSet (b:bs) =
  if elem top (b:bs)
    then top
    else foldl dis b bs
{-# NOINLINE disSet #-}

-- | Big Xor
xorSet :: [Bdd] -> Bdd
xorSet [] = bot
xorSet (b:bs) = foldl xor b bs
{-# NOINLINE xorSet #-}

-- | Greatest fixpoint for a given operator.
gfp :: (Bdd -> Bdd) -> Bdd
gfp operator = gfpStep top (operator top) where
  gfpStep :: Bdd -> Bdd -> Bdd
  gfpStep current next =
    if (current == next)
      then current
      else gfpStep next (operator next)

thenOf :: Bdd -> Bdd
thenOf b = unsafePerformIO (bdd_Then (unsafePerformIO (bdd_new 8)) b)

elseOf :: Bdd -> Bdd
elseOf b = unsafePerformIO (bdd_Else (unsafePerformIO (bdd_new 8)) b)

firstVarOf :: Bdd -> Maybe Int
firstVarOf b
  | (b == bot) = Nothing
  | (b == top) = Nothing
  | otherwise = unsafePerformIO $ do
      v <- bdd_Variable b
      return $ Just ((fromIntegral v)-(1::Int))

instance Show Bdd where
  show b = show (unravel b)

-- | A simple tree definition.
data BddTree = Bot | Top | Var Int BddTree BddTree deriving (Show,Eq)

-- | Convert a BDD to a tree.
unravel :: Bdd -> BddTree
unravel b
  | (b == bot) = Bot
  | (b == top) = Top
  | otherwise = Var n (unravel (thenOf b)) (unravel (elseOf b)) where (Just n) = firstVarOf b

-- | Convert a tree to a BDD.
ravel :: BddTree -> Bdd
ravel Bot = bot
ravel Top = top
ravel (Var n nthen nelse) = ifthenelse (var n) (ravel nthen) (ravel nelse)

-- | An assignment of boolean values to variables/integers.
type Assignment = [(Int,Bool)]

-- | Get all satisfying assignments. These will be partial, i.e. only
-- contain (a subset of) the variables that actually occur in the BDD.
allSats :: Bdd -> [Assignment]
allSats b
  | (b == bot) = []
  | (b == top) = [ [] ]
  | otherwise =
      [ (n,True):rest | rest <- allSats (thenOf b) ] ++ [ (n,False):rest | rest <- allSats (elseOf b) ]
      where (Just n) = firstVarOf b

-- | Get the lexicographically smallest satisfying assignment, if there is any.
anySat :: Bdd -> Maybe Assignment
anySat b
  | (b == bot) = Nothing
  | (b == top) = Just []
  | otherwise = Just ((n,hastobetrue):rest) where
      hastobetrue = elseOf b == bot
      (Just n)    = firstVarOf b
      (Just rest) = if hastobetrue then anySat (thenOf b) else anySat (elseOf b)

-- | Given a set of all variables, complete an assignment.
completeAss :: [Int] -> Assignment -> [Assignment]
completeAss allvars ass =
  if (addvars ass == [])
    then [ass]
    else concat $ map (completeAss allvars) (extend ass (head (addvars ass)))
  where
    addvars s = allvars \\ (sort $ map fst s)
    extend s v = [ ((v,False):s), ((v,True):s) ]

-- | Get all complete assignments, given a set of all variables.
-- In particular this will include variables not in the BDD.
allSatsWith :: [Int] -> Bdd -> [Assignment]
allSatsWith allvars b = concat $ map (completeAss allvars) (allSats b) where

-- | Given a set of all variables, get the number of satisfying assignments.
-- This should better be done without actually generating them.
satCountWith :: [Int] -> Bdd -> Int
satCountWith allvars b = length (allSatsWith allvars b)

-- | Given a set of all variables, get the lexicographically smallest complete
-- satisfying assignment, if there is any.
anySatWith :: [Int] -> Bdd -> Maybe Assignment
anySatWith allvars b = case (anySat b) of
  Nothing -> Nothing
  Just partass -> Just $ head $ completeAss allvars partass

-- | Relabel variables according to the given mapping. Note that we
-- unravel the whole BDD, hence this is an expensive operation.
relabel :: [(Int,Int)] -> Bdd -> Bdd
relabel mapping b = ravel $ relabelTree mapping (unravel b)

-- relabel variables
relabelTree :: [(Int,Int)] -> BddTree -> BddTree
relabelTree _   Top = Top
relabelTree _   Bot = Bot
relabelTree rel (Var n left right) = Var newn (relabelTree rel left) (relabelTree rel right) where
  newn = case (lookup n rel) of
	      (Just m) -> m
	      Nothing  -> n
