{-# LANGUAGE ForeignFunctionInterface #-}
module Data.HasCacBDD (
  -- types:
  Bdd, -- Assignment,
  -- creation:
  top, bot, var, -- node,
  -- combination and manipulation:
  neg, con, dis, imp, equ, xor, conSet, disSet, xorSet,
    -- forall, forallSet, exists, existsSet,
    -- restrict, restrictSet,
    -- gfp,
    -- relabel,
  -- get satisfying assignments:
    -- allSats, allSatsWith, anySat, anySatWith, satCountWith,
  -- visualization:
    -- genGraph, showGraph,
  )

where

import Data.Word
import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe

newtype Bdd = Bdd (Ptr Bdd) deriving (Show)

newtype XBddManager = XBddManager (Ptr XBddManager) deriving (Show)

foreign import ccall unsafe "BDDNodeC.h BDD_new" bdd_new :: Word -> IO Bdd
foreign import ccall "BDDNodeC.h XBDDManager_new" xBddManager_new :: CInt -> IO XBddManager
foreign import ccall "BDDNodeC.h XBDDManager_BddOne"  xBddManager_BddOne  :: Bdd -> XBddManager -> IO Bdd
foreign import ccall "BDDNodeC.h XBDDManager_BddZero" xBddManager_BddZero :: Bdd -> XBddManager -> IO Bdd
foreign import ccall unsafe "BDDNodeC.h XBDDManager_BddVar"  xBddManager_BddVar  :: Bdd -> XBddManager -> CInt -> IO Bdd

foreign import ccall "BDDNodeC.h BDD_Operator_Equal" bdd_Operator_Equal :: Bdd -> Bdd -> IO Bool

-- foreign import ccall "BDDNodeC.h BDD_Show" bdd_show :: Bdd -> IO () -- TODO later

foreign import ccall "BDDNodeC.h BDD_Operator_Not" bdd_Operator_Not :: Bdd -> Bdd -> IO Bdd
foreign import ccall "BDDNodeC.h BDD_Operator_Or" bdd_Operator_Or :: Bdd -> Bdd -> Bdd -> IO Bdd
foreign import ccall "BDDNodeC.h BDD_Operator_And" bdd_Operator_And :: Bdd -> Bdd -> Bdd -> IO Bdd

foreign import ccall "BDDNodeC.h BDD_Operator_Xor" bdd_Operator_Xor :: Bdd -> Bdd -> Bdd -> IO Bdd
-- foreign import ccall unsafe "BDDNodeC.h BDD_Operator_LessThan" bdd_Operator_LessThan :: Bdd -> Bdd -> Bdd -> IO Bdd
-- foreign import ccall unsafe "BDDNodeC.h BDD_Operator_MoreThan" bdd_Operator_MoreThan :: Bdd -> Bdd -> Bdd -> IO Bdd
foreign import ccall "BDDNodeC.h BDD_Operator_LessEqual" bdd_Operator_LessEqual :: Bdd -> Bdd -> Bdd -> IO Bdd
-- foreign import ccall unsafe "BDDNodeC.h BDD_Operator_MoreEqual" bdd_Operator_MoreEqual :: Bdd -> Bdd -> Bdd -> IO Bdd
-- foreign import ccall unsafe "BDDNodeC.h BDD_Operator_Nor" bdd_Operator_Nor :: Bdd -> Bdd -> Bdd -> IO Bdd
-- foreign import ccall unsafe "BDDNodeC.h BDD_Operator_Nand" bdd_Operator_Nand :: Bdd -> Bdd -> Bdd -> IO Bdd
-- foreign import ccall unsafe "BDDNodeC.h BDD_Operator_XNor" bdd_Operator_XNor :: Bdd -> Bdd -> Bdd -> IO Bdd

-- foreign import ccall unsafe "XBDDManager_ShowInfo" xBddManager_ShowInfo :: XBddManager -> CInt -> IO ()
-- foreign import ccall unsafe "Bdd_manager" bdd_Manager :: Bdd -> IO XBddManager

manager :: XBddManager
manager = unsafePerformIO (xBddManager_new 100) -- We fix a maximum of a 100 variables
{-# NOINLINE manager #-}

top :: Bdd
top = unsafePerformIO (xBddManager_BddOne (unsafePerformIO (bdd_new 8)) manager)
{-# NOINLINE top #-}

bot :: Bdd
bot = unsafePerformIO (xBddManager_BddZero (unsafePerformIO (bdd_new 8)) manager)
{-# NOINLINE bot #-}

var :: Int -> Bdd
var n = unsafePerformIO (xBddManager_BddVar (unsafePerformIO (bdd_new 8)) manager (fromIntegral n))
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
