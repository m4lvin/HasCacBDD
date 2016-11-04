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
  restrict, restrictSet, restrictLaw,
  ifthenelse, gfp, relabel,
  -- * Get satisfying assignments
  allSats, allSatsWith, satCountWith, anySat, anySatWith,
  -- * Sub-BDDs and length
  subOf, sizeOf,
  -- * Show and convert to trees
  BddTree(..), unravel, ravel, firstVarOf, maxVarOf, allVarsOf, thenOf, elseOf,
  -- * Print some debugging information
  maximumvar, showInfo
) where

import Foreign.C
import Foreign.Ptr (Ptr)
import Foreign (ForeignPtr, newForeignPtr, withForeignPtr, finalizerFree)
import System.IO.Unsafe
import Data.List (nub,(\\),sort)
import Data.Maybe (fromJust)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, elements, oneof, sized, listOf)

-- | The CacBDD datatype has no structure because
-- from our perspective BDDs are just pointers.
type CacBDD = ()
data Bdd = Bdd (ForeignPtr CacBDD)

-- | We attach the free() finalizer to our BDDs.
finalize :: Ptr CacBDD -> Bdd
finalize ptr = Bdd (unsafePerformIO $ newForeignPtr finalizerFree ptr)

finalizeMgr :: Ptr CacXBddManager -> XBddManager
finalizeMgr ptr = XBddManager (unsafePerformIO $ newForeignPtr finalizerFree ptr)

type CacXBddManager = ()
data XBddManager = XBddManager (ForeignPtr CacXBddManager)

type NullOp = Ptr CacBDD -> Ptr CacXBddManager -> IO (Ptr CacBDD)
type UnaryOp = Ptr CacBDD -> Ptr CacBDD -> IO (Ptr CacBDD)
type BinaryOp = Ptr CacBDD -> Ptr CacBDD -> Ptr CacBDD -> IO (Ptr CacBDD)

foreign import ccall unsafe "BDDNodeC.h BDD_new" bdd_new :: Word -> IO (Ptr CacBDD)
foreign import ccall unsafe "BDDNodeC.h XBDDManager_new" xBddManager_new :: CInt -> IO (Ptr CacXBddManager)
foreign import ccall unsafe "BDDNodeC.h XBDDManager_ShowInfo" xBddManager_showInfo :: Ptr CacXBddManager -> IO ()
foreign import ccall unsafe "BDDNodeC.h XBDDManager_BddOne"  xBddManager_BddOne  :: NullOp
foreign import ccall unsafe "BDDNodeC.h XBDDManager_BddZero" xBddManager_BddZero :: NullOp
foreign import ccall unsafe "BDDNodeC.h XBDDManager_BddVar"  xBddManager_BddVar  :: Ptr CacBDD -> Ptr CacXBddManager -> CInt -> IO (Ptr CacBDD)
foreign import ccall unsafe "BDDNodeC.h XBDDManager_Ite"     xBddManager_Ite     :: Ptr CacBDD -> Ptr CacXBddManager -> BinaryOp
foreign import ccall unsafe "BDDNodeC.h BDD_Operator_Equal"  bdd_Operator_Equal  :: Ptr CacBDD -> Ptr CacBDD -> IO Bool
foreign import ccall unsafe "BDDNodeC.h BDD_Operator_Not"    bdd_Operator_Not    :: UnaryOp
foreign import ccall unsafe "BDDNodeC.h BDD_Operator_Or"     bdd_Operator_Or     :: BinaryOp
foreign import ccall unsafe "BDDNodeC.h BDD_Operator_And"    bdd_Operator_And    :: BinaryOp
foreign import ccall unsafe "BDDNodeC.h BDD_Operator_Xor"    bdd_Operator_Xor    :: BinaryOp
foreign import ccall unsafe "BDDNodeC.h BDD_Exist"           bdd_Exist           :: BinaryOp
foreign import ccall unsafe "BDDNodeC.h BDD_Universal"       bdd_Universal       :: BinaryOp
foreign import ccall unsafe "BDDNodeC.h BDD_Restrict"        bdd_Restrict        :: BinaryOp
foreign import ccall unsafe "BDDNodeC.h BDD_Variable"        bdd_Variable        :: Ptr CacBDD -> IO CInt
foreign import ccall unsafe "BDDNodeC.h BDD_Then"            bdd_Then            :: UnaryOp
foreign import ccall unsafe "BDDNodeC.h BDD_Else"            bdd_Else            :: UnaryOp

-- | The maximum number of variables
maximumvar :: Int
maximumvar = 1048576

manager :: XBddManager
manager = finalizeMgr (unsafePerformIO $ xBddManager_new (fromIntegral maximumvar))
{-# NOINLINE manager #-}

-- | This should cover BDDOne, BddZero
fromManager :: NullOp -> Bdd
fromManager nulloperator = let (XBddManager mptr) = manager in
  finalize $ unsafePerformIO $
    withForeignPtr mptr $ nulloperator (unsafePerformIO (bdd_new 8))
{-# NOINLINE fromManager #-}

withBDD :: UnaryOp -> Bdd -> Bdd
withBDD unioperator (Bdd fptr) = finalize $ unsafePerformIO $
  withForeignPtr fptr $ unioperator (unsafePerformIO (bdd_new 8))
{-# NOINLINE withBDD #-}

withTwoBDDs :: BinaryOp -> Bdd -> Bdd -> Bdd
withTwoBDDs binoperator (Bdd fptr1) (Bdd fptr2) = finalize $ unsafePerformIO $
  withForeignPtr fptr1 $
    withForeignPtr fptr2 . binoperator (unsafePerformIO (bdd_new 8))
{-# NOINLINE withTwoBDDs #-}

fromBDD :: (Ptr CacBDD -> IO a) -> Bdd -> a
fromBDD property (Bdd fptr1) = unsafePerformIO $
  withForeignPtr fptr1 property
{-# NOINLINE fromBDD #-}

fromTwoBDDs :: (Ptr CacBDD -> Ptr CacBDD -> IO a) -> Bdd -> Bdd -> a
fromTwoBDDs binproperty (Bdd fptr1) (Bdd fptr2) = unsafePerformIO $
  withForeignPtr fptr1 $
    withForeignPtr fptr2 . binproperty
{-# NOINLINE fromTwoBDDs #-}

-- | Restrict a given variable to a given value
restrict :: Bdd -> (Int,Bool) -> Bdd
restrict b (n,bit) = withTwoBDDs bdd_Restrict b (if bit then var n else neg (var n))
{-# NOINLINE restrict #-}

-- | Restrict several variables to given values
restrictSet :: Bdd -> [(Int,Bool)] -> Bdd
restrictSet b bits = withTwoBDDs bdd_Restrict b (conSet $ map (\(n,bit) -> if bit then var n else neg (var n)) bits)
{-# NOINLINE restrictSet #-}

-- | Restrict with a law
restrictLaw :: Bdd -> Bdd -> Bdd
restrictLaw = withTwoBDDs bdd_Restrict
{-# NOINLINE restrictLaw #-}

-- | Existential Quantification
exists :: Int -> Bdd -> Bdd
exists n b = withTwoBDDs bdd_Exist b (var n)
{-# NOINLINE exists #-}

-- | Universal Quantification
forall :: Int -> Bdd -> Bdd
forall n b = withTwoBDDs bdd_Universal b (var n)
{-# NOINLINE forall #-}

-- | Big Existential Quantification
existsSet :: [Int] -> Bdd -> Bdd
existsSet ns b = foldl (flip exists) b ns

-- | Big Universal Quantification
forallSet :: [Int] -> Bdd -> Bdd
forallSet ns b = foldl (flip forall) b ns

-- | True constant
top :: Bdd
top = fromManager xBddManager_BddOne
{-# NOINLINE top #-}

-- | False constant
bot :: Bdd
bot = fromManager xBddManager_BddZero
{-# NOINLINE bot #-}

-- | Variable, indexed by any integer from 0 to 1.000.000
-- FIXME: Segfaults if n is negative or out of range.
--        Can we add a safety check without affecting performance?
var :: Int -> Bdd
var n = fromManager (\bptr mptr -> xBddManager_BddVar bptr mptr (fromIntegral (n+1)))
{-# NOINLINE var #-}

-- | If ... then ... else ...
ifthenelse :: Bdd -> Bdd -> Bdd -> Bdd
ifthenelse (Bdd test) (Bdd yes) (Bdd no) =
  let (XBddManager mptr) = manager in
    finalize $ unsafePerformIO $
      withForeignPtr test (\t ->
        withForeignPtr yes (\y ->
          withForeignPtr no (\n ->
            withForeignPtr mptr (\m -> xBddManager_Ite (unsafePerformIO (bdd_new 8)) m t y n))))
{-# NOINLINE ifthenelse #-}

instance Eq Bdd where
  b1 == b2 = same b1 b2

same :: Bdd -> Bdd -> Bool
same = fromTwoBDDs bdd_Operator_Equal
{-# NOINLINE same #-}

-- | Negation
neg :: Bdd -> Bdd
neg = withBDD bdd_Operator_Not
{-# NOINLINE neg #-}

-- | Equivalence aka Biimplication
equ :: Bdd -> Bdd -> Bdd
equ b1 b2 = con (imp b1 b2) (imp b2 b1) -- ugly...
{-# NOINLINE equ #-}

-- | Implication, via disjunction and negation.
-- Somehow this is faster than calling LessEqual?
imp :: Bdd -> Bdd -> Bdd
imp b1 = dis (neg b1)
{-# NOINLINE imp #-}

-- | Conjunction
con :: Bdd -> Bdd -> Bdd
con = withTwoBDDs bdd_Operator_And
{-# NOINLINE con #-}

-- | Disjunction
dis :: Bdd -> Bdd -> Bdd
dis = withTwoBDDs bdd_Operator_Or
{-# NOINLINE dis #-}

-- | Exclusive Or
xor :: Bdd -> Bdd -> Bdd
xor = withTwoBDDs bdd_Operator_Xor
{-# NOINLINE xor #-}

-- | Big Conjunction
conSet :: [Bdd] -> Bdd
conSet [] = top
conSet (b:bs) =
  if bot `elem` (b:bs)
    then bot
    else foldl con b bs
{-# NOINLINE conSet #-}

-- | Big Disjunction
disSet :: [Bdd] -> Bdd
disSet [] = bot
disSet (b:bs) =
  if top `elem` (b:bs)
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
    if current == next
      then current
      else gfpStep next (operator next)

thenOf :: Bdd -> Bdd
thenOf = withBDD bdd_Then

elseOf :: Bdd -> Bdd
elseOf = withBDD bdd_Else

-- | The first variable of a given BDD, if there is one.
firstVarOf :: Bdd -> Maybe Int
firstVarOf b
  | b == bot = Nothing
  | b == top = Nothing
  | otherwise = Just (fromIntegral (fromBDD bdd_Variable b) -(1::Int))

-- | The maximum variable of a given BDD, if there is one.
maxVarOf ::  Bdd -> Maybe Int
maxVarOf b
  | b == bot = Nothing
  | b == top = Nothing
  | otherwise = maximum [ Just $ fromIntegral v - (1::Int), m1, m2 ] where
      v = fromBDD bdd_Variable b
      m1 = maxVarOf $ thenOf b
      m2 = maxVarOf $ elseOf b

-- | All variables in a given BDD.
allVarsOf :: Bdd -> [Int]
allVarsOf b
  | b == bot = []
  | b == top = []
  | otherwise = sort $ nub (n : allVarsOf (thenOf b) ++ allVarsOf (elseOf b)) where (Just n) = firstVarOf b

subOf :: Bdd -> [Bdd]
subOf b
  | b == bot = []
  | b == top = []
  | otherwise = nub $ b : (subOf (thenOf b) ++ subOf (elseOf b))

sizeOf :: Bdd -> Int
sizeOf = length.subOf

-- FIXME: Should we print outermost brackets around non-constant BDDs?
instance Show Bdd where
  show b = show (unravel b)

-- TODO: add a Read instance and test that Show and Read are inverses of each other

-- | A simple tree definition to show BDDs as text.
data BddTree = Bot | Top | Var Int BddTree BddTree deriving (Show,Eq)

-- | Convert a BDD to a tree.
unravel :: Bdd -> BddTree
unravel b
  | b == bot = Bot
  | b == top = Top
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
  | b == bot = []
  | b == top = [ [] ]
  | otherwise =
      [ (n,True):rest | rest <- allSats (thenOf b) ] ++ [ (n,False):rest | rest <- allSats (elseOf b) ]
      where (Just n) = firstVarOf b

-- | Get the lexicographically smallest satisfying assignment, if there is any.
anySat :: Bdd -> Maybe Assignment
anySat b
  | b == bot = Nothing
  | b == top = Just []
  | otherwise = Just ((n,hastobetrue):rest) where
      hastobetrue = elseOf b == bot
      (Just n)    = firstVarOf b
      (Just rest) = if hastobetrue then anySat (thenOf b) else anySat (elseOf b)

-- | Given a set of all variables, complete an assignment.
completeAss :: [Int] -> Assignment -> [Assignment]
completeAss allvars ass =
  if null (addvars ass)
    then [ass]
    else concatMap (completeAss allvars) (extend ass (head (addvars ass)))
  where
    addvars s = allvars \\ sort (map fst s)
    extend s v = [ (v,False):s, (v,True):s ]

-- | Get all complete assignments, given a set of all variables.
-- In particular this will include variables not in the BDD.
allSatsWith :: [Int] -> Bdd -> [Assignment]
allSatsWith allvars b = concatMap (completeAss allvars) (allSats b)

-- | Given a set of all variables, get the number of satisfying assignments.
-- Note that allvars should be nub'd and sorted.
satCountWith :: [Int] -> Bdd -> Int
satCountWith allvars b
  | b == top = 2 ^ length allvars
  | b == bot = 0
  | otherwise = (2 ^ length varsjumped) * posbelow where
      (Just curvar) = firstVarOf b
      varsjumped = filter (< curvar) allvars
      varsleft   = filter (> curvar) allvars
      posbelow   = sum [satCountWith varsleft (branch b) | branch <- [thenOf,elseOf] ]

-- | Given a set of all variables, get the lexicographically smallest complete
-- satisfying assignment, if there is any.
anySatWith :: [Int] -> Bdd -> Maybe Assignment
anySatWith allvars b = case anySat b of
  Nothing -> Nothing
  Just partass -> Just $ head $ completeAss allvars partass

-- | Relabel variables according to the given mapping.
relabel :: [(Int,Int)] -> Bdd -> Bdd
relabel [] b = b
relabel rel@((n,newn):rest) b
  | b == bot = b
  | b == top = b
  | otherwise = case compare n (fromJust (firstVarOf b)) of
                  LT -> relabel rest b
                  EQ -> ifthenelse (var newn) (relabel rest (thenOf b)) (relabel rest (elseOf b))
                  GT -> ifthenelse (var (fromJust (firstVarOf b))) (relabel rel (thenOf b)) (relabel rel (elseOf b))

-- | Show internal statistics.
showInfo :: IO ()
showInfo = withForeignPtr mptr xBddManager_showInfo where (XBddManager mptr) = manager

-- | QuickCheck Arbitrary instances for BDDs
instance Arbitrary Bdd where
  arbitrary = sized randombdd

randombdd ::  Int -> Gen Bdd
randombdd sz = bdd sz' where
  sz' = min maximumvar sz
  bdd 0 = oneof [ pure top
                , pure bot
                , var <$> choose (0, sz')
                ]
  bdd n = oneof [ var <$> choose (0, sz')
                , neg <$> st
                , con <$> st <*> st
                , dis <$> st <*> st
                , imp <$> st <*> st
                , equ <$> st <*> st
                , xor <$> st <*> st
                , exists <$> randomvar <*> st
                , existsSet <$> listOf randomvar <*> st
                , forall <$> randomvar <*> st
                , forallSet <$> listOf randomvar <*> st
                ]
    where
      st = bdd (n `div` 2)
      randomvar = elements [0..maximumvar]
