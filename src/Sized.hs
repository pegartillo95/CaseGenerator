{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Sized
    ( Allv(..)
      , Sized(..)
      , compose
    ) where

import GHC.Generics
import qualified Data.Set as S
import qualified Data.PQueue.Min as Q
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map
import Arrays
import Unsafe.TrueName

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
  allv = [1..5]

instance Sized Char where
  size x = 1

instance Allv Char where
  allv = ['a'..'z']

instance Sized Bool where
  size x = 1

instance Allv Bool where
   allv = [True, False]

instance Sized a => Sized [a]

instance (Sized a, Sized b) => Sized (a,b)

instance (Sized a, Sized b, Sized c) => Sized (a,b,c)

instance (Sized a, Sized b, Sized c, Sized d) => Sized (a,b,c,d)

instance (Sized a, Sized b, Sized c, Sized d, Sized e) => Sized (a,b,c,d,e)

instance (Sized a, Sized b, Sized c, Sized d, Sized e, Sized f) => Sized (a,b,c,d,e,f)

instance Allv a => Allv [a] where
   allv = [] : (map (\(x,xs) -> x:xs) $ compose allv allv)

instance (Allv a, Allv b) => Allv (a,b) where
   allv = compose allv allv

instance (Allv a, Allv b, Allv c) => Allv (a,b,c) where
   allv = map (\(x,(y,z)) -> (x,y,z)) (compose allv (compose allv allv))

instance (Allv a, Allv b, Allv c, Allv d) => Allv (a,b,c,d) where
   allv = map (\(x,(y,(z,t))) -> (x,y,z,t)) (compose allv (compose allv (compose allv allv)))

instance (Allv a, Allv b, Allv c, Allv d, Allv e) => Allv (a,b,c,d,e) where
   allv = map (\(x,(y,(z,(t,u)))) -> (x,y,z,t,u)) (compose allv (compose allv (compose allv (compose allv allv))))

instance (Allv a, Allv b, Allv c, Allv d, Allv e, Allv f) => Allv (a,b,c,d,e,f) where
   allv = map (\(x,(y,(z,(t,(u,v))))) -> (x,y,z,t,u,v)) (compose allv (compose allv (compose allv (compose allv (compose allv allv)))))



instance Allv a => Allv (Array a) where
   allv = map buildArray allv where
          buildArray xs = Arr (Map.fromList $ zip [0..] xs, length xs)

instance Sized a => Sized (Array a) where
   size (Arr (_,len)) = len


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

------------------------------------------
--------Composing of 2 lists--------------
------------------------------------------

compose :: [a] -> [b] -> [(a,b)]
compose xs ys = allpairs
  where allpairs = concat $ diags 0 xs ys

--
-- It builds the lattice of tuples from two lists, each one may be either
-- finite or infinite


diags :: Int -> [a] -> [b] -> [[(a,b)]]
diags _ [] [] = [[]]
diags i xs ys
    | fullDiag     = [tup k | k <- [0..i]] : diags (i+1) xs ys
    | finiteFirst  = diags (i-1) xs  ysr
    | finiteSecond = diags (i-1) xsr ys
    | otherwise    = diags (i-2) xsr ysr

  where xs'          = drop i xs
        ys'          = drop i ys
        xsr          = tail xs
        ysr          = tail ys
        fullDiag     = not (null xs') && not (null ys')
        finiteFirst  = null xs' && not (null ys')
        finiteSecond = not (null xs') && null ys'
        tup k        = (x,y)
                       where x = xs !! k 
                             y = ys !! (i-k)