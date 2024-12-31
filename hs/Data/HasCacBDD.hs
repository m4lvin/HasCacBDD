-- | Haskell bindings for CacBDD, a BDD Package with Dynamic Cache Management.

{-# LANGUAGE ForeignFunctionInterface #-}
module Data.HasCacBDD (
  -- * Types
  Bdd, Assignment,
  -- * Creation of new BDDs
  top, bot, var,
  -- * Combination and Manipulation of BDDs
  neg, con, dis, imp, equ, xor, conSet, disSet, xorSet,
  exists_, forall_, existsSet, forallSet,
  restrict, restrictSet, restrictLaw,
  ifthenelse, gfp, relabel, relabelFun,
  substit, substitSimul,
  -- * Evaluation
  evaluate, evaluateFun,
  -- * Get satisfying assignments
  allSats, allSatsWith, satCountWith, anySat, anySatWith,
  -- * Variables
  firstVarOf, maxVarOf, allVarsOf, allVarsOfSorted,
  -- * Sub-BDDs and length
  thenOf, elseOf, subsOf, sizeOf,
  -- * Variable Orderings
  optimalOrder,
  -- * Show and convert to trees
  BddTree(..), unravel, ravel,
  -- * Print some debugging information
  maximumvar, showInfo
) where

import Control.Arrow (Arrow(first))
import Foreign.C (CInt(..))
import Foreign.Ptr (Ptr)
import Foreign (ForeignPtr, newForeignPtr, withForeignPtr, finalizerFree)
import System.IO.Unsafe (unsafePerformIO)
import Data.Function (on)
import Data.List ((\\), minimumBy, nub, permutations, sort)
import Data.Maybe (fromJust)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, shrink, choose, oneof, sized, listOf)

-- | The CacBDD datatype has no structure because
-- from our perspective BDDs are just pointers.
newtype Bdd = Bdd (ForeignPtr CacBDD)
type CacBDD = ()

-- | An assignment of boolean values to variables/integers.
type Assignment = [(Int,Bool)]

finalize :: Ptr CacBDD -> Bdd
finalize ptr = Bdd (unsafePerformIO $ newForeignPtr finalizerFree ptr)

finalizeMgr :: Ptr CacXBddManager -> XBddManager
finalizeMgr ptr = XBddManager (unsafePerformIO $ newForeignPtr finalizerFree ptr)

type CacXBddManager = ()
newtype XBddManager = XBddManager (ForeignPtr CacXBddManager)

type NullOp = Ptr CacBDD -> Ptr CacXBddManager -> IO ()
type UnaryOp = Ptr CacBDD -> Ptr CacBDD -> IO ()
type BinaryOp = Ptr CacBDD -> Ptr CacBDD -> Ptr CacBDD -> IO ()

foreign import ccall unsafe "BDDNodeC.h BDD_new" bdd_new :: IO (Ptr CacBDD)
foreign import ccall unsafe "BDDNodeC.h XBDDManager_new" xBddManager_new :: CInt -> IO (Ptr CacXBddManager)
foreign import ccall unsafe "BDDNodeC.h XBDDManager_ShowInfo" xBddManager_showInfo :: Ptr CacXBddManager -> IO ()
foreign import ccall unsafe "BDDNodeC.h XBDDManager_BddOne"  xBddManager_BddOne  :: NullOp
foreign import ccall unsafe "BDDNodeC.h XBDDManager_BddZero" xBddManager_BddZero :: NullOp
foreign import ccall unsafe "BDDNodeC.h XBDDManager_BddVar"  xBddManager_BddVar  :: Ptr CacBDD -> Ptr CacXBddManager -> CInt -> IO ()
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

fromManager :: NullOp -> Bdd
fromManager nulloperator = let (XBddManager mptr) = manager in
  finalize $ unsafePerformIO $ do
    b <- bdd_new
    withForeignPtr mptr $ nulloperator b
    return b
{-# NOINLINE fromManager #-}

withBDD :: UnaryOp -> Bdd -> Bdd
withBDD unioperator (Bdd fptr) = finalize $ unsafePerformIO $ do
  b <- bdd_new
  withForeignPtr fptr $ unioperator b
  return b
{-# NOINLINE withBDD #-}

withTwoBDDs :: BinaryOp -> Bdd -> Bdd -> Bdd
withTwoBDDs binoperator (Bdd fptr1) (Bdd fptr2) = finalize $ unsafePerformIO $ do
  b <- bdd_new
  withForeignPtr fptr1 $
    withForeignPtr fptr2 . binoperator b
  return b
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

-- | Restrict a single variable to a given value
restrict :: Bdd -> (Int,Bool) -> Bdd
restrict b (n,bit) = withTwoBDDs bdd_Restrict b (if bit then var n else neg (var n))
{-# NOINLINE restrict #-}

-- | Restrict with a (partial) assignment
restrictSet :: Bdd -> Assignment -> Bdd
restrictSet b bits = withTwoBDDs bdd_Restrict b (conSet $ map (\(n,bit) -> if bit then var n else neg (var n)) bits)
{-# NOINLINE restrictSet #-}

-- | Restrict with a law. Note that the law is the second parameter!
restrictLaw :: Bdd -> Bdd -> Bdd
restrictLaw = withTwoBDDs bdd_Restrict
{-# NOINLINE restrictLaw #-}

-- | Existential Quantification
exists_ :: Int -> Bdd -> Bdd
exists_ n b = withTwoBDDs bdd_Exist b (var n)
{-# NOINLINE exists_ #-}

-- | Universal Quantification
forall_ :: Int -> Bdd -> Bdd
forall_ n b = withTwoBDDs bdd_Universal b (var n)
{-# NOINLINE forall_ #-}

-- | Big Existential Quantification
existsSet :: [Int] -> Bdd -> Bdd
existsSet ns b = foldl (flip exists_) b ns

-- | Big Universal Quantification
forallSet :: [Int] -> Bdd -> Bdd
forallSet ns b = foldl (flip forall_) b ns

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
    finalize $ unsafePerformIO $ do
      b <- bdd_new
      withForeignPtr test (\t ->
        withForeignPtr yes (\y ->
          withForeignPtr no (\n ->
            withForeignPtr mptr (\m -> xBddManager_Ite b m t y n))))
      return b
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

-- | Then-branch of a given BDD, setting firstVarOf to True.
thenOf :: Bdd -> Bdd
thenOf = withBDD bdd_Then

-- | Else-branch of a given BDD, setting firstVarOf to False.
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

-- | All variables in a given BDD, /not/ sorted, lazy.
allVarsOf :: Bdd -> [Int]
allVarsOf b
  | b == bot = []
  | b == top = []
  | otherwise =
      let n = fromJust (firstVarOf b)
      in n : nub (allVarsOf (thenOf b) ++ allVarsOf (elseOf b))

-- | All variables in a given BDD, sorted, *not* lazy.
allVarsOfSorted :: Bdd -> [Int]
allVarsOfSorted = sort . allVarsOf

-- | List all node / sub-BDDs of a given BDD.
-- This includes the root node, but omits terminal nodes.
subsOf :: Bdd -> [Bdd]
subsOf = subsOf' [] where
  subsOf' done b
    | b == bot = done
    | b == top = done
    | b `elem` done = done
    | otherwise     = let intermedDone = subsOf' done (thenOf b)
                       in b : subsOf' intermedDone (elseOf b)

-- | Size of the BDD, the number of non-terminal nodes.
sizeOf :: Bdd -> Int
sizeOf = length . subsOf

instance Show Bdd where
  show = show . unravel

instance Read Bdd where
  readsPrec k input = map (first ravel) (readsPrec k input)

-- | A simple tree definition to show BDDs as text.
data BddTree = Bot | Top | Var Int BddTree BddTree deriving (Eq,Read,Show)

-- | Convert a BDD to a tree.
unravel :: Bdd -> BddTree
unravel b
  | b == bot = Bot
  | b == top = Top
  | otherwise = Var n (unravel (thenOf b)) (unravel (elseOf b)) where n = fromJust $ firstVarOf b

-- | Convert a tree to a BDD.
ravel :: BddTree -> Bdd
ravel Bot = bot
ravel Top = top
ravel (Var n nthen nelse) = ifthenelse (var n) (ravel nthen) (ravel nelse)

-- | Evaluate a BDD given an assignment.
-- Returns Nothing if the assignment does not cover allVarsOf b.
evaluate :: Bdd -> Assignment -> Maybe Bool
evaluate b ass =
  if all (`elem` map fst ass) (allVarsOf b)
    then Just $ top == restrictSet b ass
    else Nothing

-- | Evaluate a BDD given a total assignment function.
evaluateFun :: Bdd -> (Int -> Bool) -> Bool
evaluateFun b f
  | b == bot = False
  | b == top = True
  | otherwise =
      let n = fromJust $ firstVarOf b
      in evaluateFun ((if f n then thenOf else elseOf) b) f

-- | Get all satisfying assignments. These will be partial, i.e. only
-- contain (a subset of) the variables that actually occur in the BDD.
allSats :: Bdd -> [Assignment]
allSats b
  | b == bot = []
  | b == top = [ [] ]
  | otherwise =
      [ (n,True):rest | rest <- allSats (thenOf b) ] ++ [ (n,False):rest | rest <- allSats (elseOf b) ]
      where n = fromJust $ firstVarOf b

-- | Get the lexicographically smallest satisfying assignment, if there is any.
anySat :: Bdd -> Maybe Assignment
anySat b
  | b == bot = Nothing
  | b == top = Just []
  | otherwise = Just ((n,hastobetrue):rest) where
      hastobetrue = elseOf b == bot
      n = fromJust $ firstVarOf b
      rest = fromJust $ if hastobetrue then anySat (thenOf b) else anySat (elseOf b)

-- | Given a set of all variables, complete an assignment.
completeAss :: [Int] -> Assignment -> [Assignment]
completeAss allvars ass =
  if null (addvars ass)
    then [ass]
    else concatMap (completeAss allvars) (extend ass (head (addvars ass)))
  where
    addvars s = allvars \\ sort (map fst s)
    extend s v = [ (v,False):s, (v,True):s ]

-- | Get all complete assignments, given a list of variables.
-- In particular this will include variables not in the BDD.
allSatsWith :: [Int] -> Bdd -> [Assignment]
allSatsWith allvars b = concatMap (completeAss allvars) (allSats b)

-- | Get the number of satisfying assignments, given a list of variables.
-- Note that the given list must be nub'd and sorted.
satCountWith :: [Int] -> Bdd -> Int
satCountWith allvars b
  | b == top = 2 ^ length allvars
  | b == bot = 0
  | otherwise = (2 ^ length varsjumped) * posbelow where
      curvar = fromJust $ firstVarOf b
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
-- Note that the mapping list must be sorted!
relabel :: [(Int,Int)] -> Bdd -> Bdd
relabel [] b = b
relabel rel@((n,newn):rest) b
  | b == bot = b
  | b == top = b
  | otherwise = case compare n (fromJust (firstVarOf b)) of
                  LT -> relabel rest b
                  EQ -> ifthenelse (var newn) (relabel rest (thenOf b)) (relabel rest (elseOf b))
                  GT -> ifthenelse (var (fromJust (firstVarOf b))) (relabel rel (thenOf b)) (relabel rel (elseOf b))

-- | Relabel variables according to the given function.
relabelFun :: (Int -> Int) -> Bdd -> Bdd
relabelFun f b = case firstVarOf b of
  Nothing -> b
  Just m  -> ifthenelse (var (f m)) (relabelFun f (thenOf b)) (relabelFun f (elseOf b))

-- | Substitute a BDD for a given variable in another BDD.
substit :: Int -> Bdd -> Bdd -> Bdd
substit n psi b =
  case firstVarOf b of
    Nothing -> b
    Just k  -> case compare n k of
      LT -> b
      EQ -> ifthenelse psi (thenOf b) (elseOf b)
      GT -> ifthenelse (var k) (substit n psi (thenOf b)) (substit n psi (elseOf b))

-- | Simultaneous substitution of BDDs for variables.
-- Note that this is not the same as folding `substit`.
substitSimul :: [(Int,Bdd)] -> Bdd -> Bdd
substitSimul []    b = b
substitSimul repls b =
  case firstVarOf b of
    Nothing -> b
    Just k  -> case lookup k repls of
      Nothing  -> ifthenelse (var k) (substitSimul repls $ thenOf b) (substitSimul repls $ elseOf b)
      Just psi -> ifthenelse psi     (substitSimul repls $ thenOf b) (substitSimul repls $ elseOf b)

-- | Find an optimal variable-reording.
-- Returns a relabelling @r@ such that @sizeOf (relabel r b)@ is minimal.
optimalOrder :: Bdd -> [(Int,Int)]
optimalOrder b = minimumBy (compare `on` (\r -> sizeOf (relabel r b))) allPermut where
  allPermut = map (zip (allVarsOf b)) $ permutations (allVarsOf b)

-- | Show internal statistics.
showInfo :: IO ()
showInfo = withForeignPtr mptr xBddManager_showInfo where (XBddManager mptr) = manager

-- | QuickCheck Arbitrary instances for BDDs
instance Arbitrary Bdd where
  arbitrary = sized randombdd
  shrink b | b == top  = []
           | b == bot  = []
           | otherwise = [thenOf b, elseOf b]

randomvarnumber :: Gen Int
randomvarnumber = choose (0, 100)

randombdd ::  Int -> Gen Bdd
randombdd 0 = oneof
  [ pure top
  , pure bot
  , var <$> randomvarnumber
  ]
randombdd n = oneof
  [ var <$> randomvarnumber
  , neg <$> smallerbdd
  , con <$> smallerbdd <*> smallerbdd
  , dis <$> smallerbdd <*> smallerbdd
  , imp <$> smallerbdd <*> smallerbdd
  , equ <$> smallerbdd <*> smallerbdd
  , xor <$> smallerbdd <*> smallerbdd
  , exists_ <$> randomvarnumber <*> smallerbdd
  , existsSet <$> listOf randomvarnumber <*> smallerbdd
  , forall_ <$> randomvarnumber <*> smallerbdd
  , forallSet <$> listOf randomvarnumber <*> smallerbdd
  ]
  where
    smallerbdd = randombdd (n `div` 2)
