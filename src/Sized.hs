{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}

module Sized
    ( Allv(..)
      , Sized(..)
    ) where

import GHC.Generics
import qualified Data.Set as S
import qualified Data.PQueue.Min as Q
import System.IO.Unsafe (unsafePerformIO)

------------------------Num test cases------------------------
uutNumCases :: Int
uutNumCases = 1000

--Class Allv
class Allv a where
  allv :: [a]

-- | For basic types we must give the instances
instance Sized Int where
  size x = 1

instance Allv Int where
  allv = [1..100]

instance Sized Char where
  size x = 1

instance Allv Char where
  allv = ['a'..'z']

instance Sized Bool where
  size x = 1

instance Allv Bool where
   allv = [True, False]  



-- | This is the exported, visible class that inherits from Allv.
class (Allv a) => Sized a where
  -- This function returns the first n elements of size lower or equal m from the "allv" list
  sized::Int->Int->[a]
  sized n m = take n (filter (\x-> (size x) <= m) allv)

  --This function takes an integer n and returns the n first elements of the "allv" list
  smallest :: [a]
  smallest = take uutNumCases (concat $ repeat allv)



  size ::  a -> Int
  default size :: (Generic a, GSized (Rep a)) => a -> Int
  size a = gsize (from a)

-- | This is the generic, non-visible class
class GSized f where
  gsize :: f a -> Int

-- | Unit: used for constructors without arguments
instance GSized U1 where
  gsize U1 = 0

-- | Products: encode multiple arguments to constructors
instance (GSized a, GSized b) => GSized (a :*: b) where
  gsize (x :*: y) = gsize x + gsize y

-- | Sums: encode choice between constructors
instance (GSized a, GSized b) => GSized (a :+: b) where
  gsize (L1 x) = gsize x
  gsize (R1 x) = gsize x

-- | Meta-information (constructor names, etc.)
instance (GSized f) => GSized (M1 i c f) where
  gsize (M1 x) = gsize x

-- | Constants, additional parameters and recursion of kind *
instance Sized a => GSized (K1 i a) where
  gsize (K1 x) = size x

