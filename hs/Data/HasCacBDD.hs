{-# LANGUAGE ForeignFunctionInterface #-}
module Data.HasCacBDD (
  -- types:
  Bdd, -- Assignment,
  -- creation:
  top, bot, var, -- node,
  -- combination and manipulation:
  neg, con, dis, imp, equ, xor, conSet, disSet, xorSet,
  exists, forall, forallSet, existsSet,
  restrict, restrictSet,
  gfp,
    -- relabel,
  -- get satisfying assignments:
  allSats, allSatsWith, satCountWith, -- anySat, anySatWith,
  -- information and visualization:
    info -- genGraph, showGraph,
  )

where

import Data.Word
import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe
import Data.List

newtype Bdd = Bdd (Ptr Bdd) deriving (Show)

newtype XBddManager = XBddManager (Ptr XBddManager) deriving (Show)

foreign import ccall unsafe "BDDNodeC.h BDD_new" bdd_new :: Word -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h XBDDManager_new" xBddManager_new :: CInt -> IO XBddManager
foreign import ccall unsafe "BDDNodeC.h XBDDManager_BddOne"  xBddManager_BddOne  :: Bdd -> XBddManager -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h XBDDManager_BddZero" xBddManager_BddZero :: Bdd -> XBddManager -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h XBDDManager_BddVar"  xBddManager_BddVar  :: Bdd -> XBddManager -> CInt -> IO Bdd

foreign import ccall "BDDNodeC.h BDD_Operator_Equal" bdd_Operator_Equal :: Bdd -> Bdd -> IO Bool

-- foreign import ccall "BDDNodeC.h BDD_Show" bdd_show :: Bdd -> IO () -- TODO later

foreign import ccall unsafe "BDDNodeC.h BDD_Operator_Not" bdd_Operator_Not :: Bdd -> Bdd -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h BDD_Operator_Or" bdd_Operator_Or :: Bdd -> Bdd -> Bdd -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h BDD_Operator_And" bdd_Operator_And :: Bdd -> Bdd -> Bdd -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h BDD_Operator_Xor" bdd_Operator_Xor :: Bdd -> Bdd -> Bdd -> IO Bdd
-- foreign import ccall unsafe "BDDNodeC.h BDD_Operator_LessThan" bdd_Operator_LessThan :: Bdd -> Bdd -> Bdd -> IO Bdd
-- foreign import ccall unsafe "BDDNodeC.h BDD_Operator_MoreThan" bdd_Operator_MoreThan :: Bdd -> Bdd -> Bdd -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h BDD_Operator_LessEqual" bdd_Operator_LessEqual :: Bdd -> Bdd -> Bdd -> IO Bdd
-- foreign import ccall unsafe "BDDNodeC.h BDD_Operator_MoreEqual" bdd_Operator_MoreEqual :: Bdd -> Bdd -> Bdd -> IO Bdd
-- foreign import ccall unsafe "BDDNodeC.h BDD_Operator_Nor" bdd_Operator_Nor :: Bdd -> Bdd -> Bdd -> IO Bdd
-- foreign import ccall unsafe "BDDNodeC.h BDD_Operator_Nand" bdd_Operator_Nand :: Bdd -> Bdd -> Bdd -> IO Bdd
-- foreign import ccall unsafe "BDDNodeC.h BDD_Operator_XNor" bdd_Operator_XNor :: Bdd -> Bdd -> Bdd -> IO Bdd

foreign import ccall unsafe "BDDNodeC.h BDD_Exist"     bdd_Exist :: Bdd -> Bdd -> Bdd -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h BDD_Universal" bdd_Universal :: Bdd -> Bdd -> Bdd -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h BDD_Restrict"  bdd_Restrict :: Bdd -> Bdd -> Bdd -> IO Bdd

restrict :: Bdd -> (Int,Bool) -> Bdd
restrict b (n,bit) = unsafePerformIO $ bdd_Restrict (unsafePerformIO (bdd_new 8)) b res where
  res = if bit then (var n) else neg (var n)
{-# NOINLINE restrict #-}

restrictSet :: Bdd -> [(Int,Bool)] -> Bdd
restrictSet b bits = unsafePerformIO $ bdd_Restrict (unsafePerformIO (bdd_new 8)) b res where
  res = conSet $ map (\(n,bit) -> if bit then (var n) else neg (var n)) bits
{-# NOINLINE restrictSet #-}

exists :: Int -> Bdd -> Bdd
exists n b = unsafePerformIO $ bdd_Exist (unsafePerformIO (bdd_new 8)) b (var n)
{-# NOINLINE exists #-}

forall :: Int -> Bdd -> Bdd
forall n b = unsafePerformIO $ bdd_Universal (unsafePerformIO (bdd_new 8)) b (var n)
{-# NOINLINE forall #-}

forallSet :: [Int] -> Bdd -> Bdd
forallSet ns b = foldl (flip forall) b ns

existsSet :: [Int] -> Bdd -> Bdd
existsSet ns b = foldl (flip exists) b ns

-- foreign import ccall unsafe "XBDDManager_ShowInfo" xBddManager_ShowInfo :: XBddManager -> CInt -> IO ()
-- foreign import ccall unsafe "Bdd_manager" bdd_Manager :: Bdd -> IO XBddManager

manager :: XBddManager
manager = unsafePerformIO (xBddManager_new 1048576) -- fix the number of variables
{-# NOINLINE manager #-}

top :: Bdd
top = unsafePerformIO (xBddManager_BddOne (unsafePerformIO (bdd_new 8)) manager)
{-# NOINLINE top #-}

bot :: Bdd
bot = unsafePerformIO (xBddManager_BddZero (unsafePerformIO (bdd_new 8)) manager)
{-# NOINLINE bot #-}

var :: Int -> Bdd
var n = unsafePerformIO (xBddManager_BddVar (unsafePerformIO (bdd_new 8)) manager (fromIntegral (n+1)))
{-# NOINLINE var #-}

instance Eq Bdd where
  b1 == b2 = same b1 b2

same :: Bdd -> Bdd -> Bool
same b1 b2 = unsafePerformIO (bdd_Operator_Equal b1 b2)
{-# NOINLINE same #-}

equ :: Bdd -> Bdd -> Bdd
equ b1 b2 = con (imp b1 b2) (imp b2 b1) -- ugly...
{-# NOINLINE equ #-}

imp :: Bdd -> Bdd -> Bdd
imp b1 b2 = unsafePerformIO (bdd_Operator_LessEqual (unsafePerformIO (bdd_new 8)) b1 b2)
{-# NOINLINE imp #-}

neg :: Bdd -> Bdd
neg b = unsafePerformIO (bdd_Operator_Not (unsafePerformIO (bdd_new 8)) b)
{-# NOINLINE neg #-}

con :: Bdd -> Bdd -> Bdd
con b1 b2 = unsafePerformIO (bdd_Operator_And (unsafePerformIO (bdd_new 8)) b1 b2)
{-# NOINLINE con #-}

dis :: Bdd -> Bdd -> Bdd
dis b1 b2 = unsafePerformIO (bdd_Operator_Or (unsafePerformIO (bdd_new 8)) b1 b2)
{-# NOINLINE dis #-}

xor :: Bdd -> Bdd -> Bdd
xor b1 b2 = unsafePerformIO (bdd_Operator_Xor (unsafePerformIO (bdd_new 8)) b1 b2)
{-# NOINLINE xor #-}

conSet :: [Bdd] -> Bdd
conSet [] = top
conSet (b:bs) =
  if elem bot (b:bs)
    then bot
    else foldl con b bs

disSet :: [Bdd] -> Bdd
disSet [] = bot
disSet (b:bs) =
  if elem top (b:bs)
    then top
    else foldl dis b bs

xorSet :: [Bdd] -> Bdd
xorSet [] = bot
xorSet (b:bs) = foldl xor b bs

-- greatest fixedpoint for a given operator
gfp :: (Bdd -> Bdd) -> Bdd
gfp operator = gfpStep top (operator top) where
  gfpStep :: Bdd -> Bdd -> Bdd
  gfpStep current next =
    if (current == next)
      then current
      else gfpStep next (operator next)

foreign import ccall "BDDNodeC.h BDD_IsComp" bdd_isBot :: Bdd -> IO Bool
foreign import ccall "BDDNodeC.h BDD_Variable" bdd_Variable :: Bdd -> IO CInt
foreign import ccall "BDDNodeC.h BDD_Then" bdd_Then :: Bdd -> Bdd -> IO Bdd
foreign import ccall "BDDNodeC.h BDD_Else" bdd_Else :: Bdd -> Bdd -> IO Bdd

thenOf :: Bdd -> Bdd
thenOf b = unsafePerformIO (bdd_Then (unsafePerformIO (bdd_new 8)) b)

elseOf :: Bdd -> Bdd
elseOf b = unsafePerformIO (bdd_Else (unsafePerformIO (bdd_new 8)) b)

-- seems to be fixed in CacBDD ...
topvar :: CInt
topvar = 2147483647 -- 1024^3 * 2 - 1

info :: Bdd -> String
info b = unsafePerformIO $ do
  comp <- bdd_isBot b
  if comp then
    return "bot"
  else do
    v <- bdd_Variable b
    if v==topvar then
      return "top"
    else
      return $ ("(" ++ (show ((fromIntegral v)-(1::Integer))))
	++ " ? " ++ (info $ thenOf b) ++ " : " ++ (info $ elseOf b) ++ ")"

type Assignment = [(Int,Bool)]

allSats :: Bdd -> [Assignment]
allSats b = unsafePerformIO $ do
  isbot <- bdd_isBot b
  if isbot then
    return [ ]
  else do
    v <- bdd_Variable b
    if v==topvar then
      return [ [] ]
    else do
      let n = (fromIntegral v)-(1::Int)
      return $ [ (n,True):rest | rest <- allSats (thenOf b) ]
	++ [ (n,False):rest | rest <- allSats (elseOf b) ]

-- -- find the lexicographically smallest satisfying assignment
-- anySat :: Bdd -> Maybe Assignment
-- anySat b =

-- given a set of all vars, complete an assignment
completeAss :: [Int] -> Assignment -> [Assignment]
completeAss allvars ass =
  if (addvars ass == [])
    then [ass]
    else concat $ map (completeAss allvars) (extend ass (head (addvars ass)))
  where
    addvars s = allvars \\ (sort $ map fst s)
    extend s v = [ ((v,False):s), ((v,True):s) ]

-- given a set of all vars, get all complete assignments
-- (including those which might have disappeared in the BDD)
allSatsWith :: [Int] -> Bdd -> [Assignment]
allSatsWith allvars b = concat $ map (completeAss allvars) (allSats b) where

-- given a set of all vars, get the number of satisfying assignments
-- this is not efficient and could be done without actually generating them!
satCountWith :: [Int] -> Bdd -> Int
satCountWith allvars b = length (allSatsWith allvars b)
