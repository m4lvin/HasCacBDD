{-

This module is a stripped-down version of Data.HasCacBDD trying to
debug the segfault only happening on M1 and arm aarch64.

Main changes:

- not using ForeignPtr but just Ptr
- unsafePerformIO is not used here

-}

{-# LANGUAGE ForeignFunctionInterface #-}

module Data.HasCacBDD.Safe where

import Foreign.C (CInt(..))
import Foreign.Ptr (Ptr)
  
-- | The CacBDD datatype has no structure because
-- from our perspective BDDs are just pointers.
data CacBDD
type Bdd = Ptr CacBDD

-- | An assignment of boolean values to variables/integers.
type Assignment = [(Int,Bool)]

data CacXBddManager
type XBddManager = Ptr CacXBddManager

-- The first pointer is for the result/output.
type NullOp = Ptr CacBDD -> Ptr CacXBddManager -> IO ()
type UnaryOp = Ptr CacBDD -> Ptr CacBDD -> IO ()
type BinaryOp = Ptr CacBDD -> Ptr CacBDD -> Ptr CacBDD -> IO ()

-- QUESTION: why do UnaryOp and BinaryOp not need the manager?

foreign import ccall unsafe "BDDNodeC.h BDD_new"              bdd_new :: IO (Ptr CacBDD) -- Why additional "Word" parameter before?
foreign import ccall unsafe "BDDNodeC.h XBDDManager_new"      xBddManager_new :: CInt -> IO (Ptr CacXBddManager)
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

newManager :: IO XBddManager
newManager = xBddManager_new (fromIntegral maximumvar)
{-# NOINLINE newManager #-}

fromManager :: NullOp -> IO Bdd
fromManager nulloperator = do
  m <- newManager -- Weird!!
  putStrLn $ "fromManager: " ++ show m -- debug
  b <- bdd_new
  nulloperator b m
  return b
{-# NOINLINE fromManager #-}

withBDD :: UnaryOp -> Bdd -> IO Bdd
withBDD unioperator fptr = do
  b <- bdd_new
  unioperator b fptr
  return b
{-# NOINLINE withBDD #-}

withTwoBDDs :: BinaryOp -> Bdd -> Bdd -> IO Bdd
withTwoBDDs binoperator fptr1 fptr2 = do
  b <- bdd_new
  binoperator b fptr1 fptr2
  return b
{-# NOINLINE withTwoBDDs #-}

fromBDD :: (Ptr CacBDD -> IO a) -> Bdd -> IO a
fromBDD = id 
{-# NOINLINE fromBDD #-}

fromTwoBDDs :: (Ptr CacBDD -> Ptr CacBDD -> IO a) -> Bdd -> Bdd -> IO a
fromTwoBDDs = id
{-# NOINLINE fromTwoBDDs #-}

-- | True constant
top :: IO Bdd
top = fromManager xBddManager_BddOne
{-# NOINLINE top #-}

-- | False constant
bot :: IO Bdd
bot = fromManager xBddManager_BddZero
{-# NOINLINE bot #-}

-- | Variable, indexed by any integer from 0 to 1.000.000
-- FIXME: Segfaults if n is negative or out of range.
--        Can we add a safety check without affecting performance?
var :: Int -> IO Bdd
var n = fromManager (\bptr mptr -> xBddManager_BddVar bptr mptr (fromIntegral (n+1)))
{-# NOINLINE var #-}

same :: Bdd -> Bdd -> IO Bool
same = fromTwoBDDs bdd_Operator_Equal
{-# NOINLINE same #-}

-- | Negation
neg :: Bdd -> IO Bdd
neg = withBDD bdd_Operator_Not
{-# NOINLINE neg #-}

-- | Equivalence aka Biimplication
equ :: Bdd -> Bdd -> IO Bdd
equ b1 b2 = imp b1 b2 >>= \x -> imp b2 b1 >>= \y -> con x y
{-# NOINLINE equ #-}

-- | Implication, via disjunction and negation.
-- Somehow this is faster than calling LessEqual?
imp :: Bdd -> Bdd -> IO Bdd
imp b1 b2 = neg b1 >>= dis b2
{-# NOINLINE imp #-}

-- | Conjunction
con :: Bdd -> Bdd -> IO Bdd
con = withTwoBDDs bdd_Operator_And
{-# NOINLINE con #-}

-- | Disjunction
dis :: Bdd -> Bdd -> IO Bdd
dis = withTwoBDDs bdd_Operator_Or
{-# NOINLINE dis #-}
