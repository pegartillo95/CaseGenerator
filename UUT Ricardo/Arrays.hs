{-# LANGUAGE DeriveDataTypeable #-}

module Arrays where 

import qualified Data.Map.Lazy as M
import qualified Data.List as L
import Data.Maybe
import Control.Exception
import Data.Typeable

type Array a = (M.Map Int a, Int)

data ArraysException = IndexOutOfRange Int
   deriving (Show, Typeable)
instance Exception ArraysException

-- Ask for the element in a position
get::Array a -> Int -> a
get a i = if isNothing e then throw (IndexOutOfRange i) else fromJust e
   where e = M.lookup i (fst a)

-- Set an element in a position
set::Array a -> Int -> a -> Array a
set a i v = if i>=0 && i<(snd a) then (M.insert i v (fst a), snd a) else throw (IndexOutOfRange i)

-- True if sorted subarray (in ascending order)
sorted_sub::Ord a => Array a -> Int -> Int -> Bool
sorted_sub a l u = if l>=(snd a) then throw (IndexOutOfRange l) else 
   if u>(snd a) then throw (IndexOutOfRange u) else sorted_aux (L.take (u-l) (L.drop l (M.elems (fst a))))
      where 
         sorted_aux [] = True
         sorted_aux [x] = True
         sorted_aux (a:b:xs) = a<=b && sorted_aux (b:xs)

-- True if sorted array (in ascending order)
sorted::Ord a => Array a -> Bool
sorted a = sorted_sub a 0 (snd a)

-- Subarrays equality
array_eq_sub::Eq a => Array a -> Array a -> Int -> Int -> Bool
array_eq_sub a1 a2 l u = if l>=(snd a1) || l>=(snd a2) then throw (IndexOutOfRange l) else 
   if u>(snd a1) || u>(snd a2) then throw (IndexOutOfRange u) else M.null (M.differenceWithKey f (fst a1) (fst a2))
   where f k a b = if (k<l) || (k>=u) || (a==b) then Nothing else Just a

-- Arrays equality
array_eq::Eq a => Array a -> Array a -> Bool
array_eq a1 a2 = (snd a1) == (snd a2) && array_eq_sub a1 a2 0 (snd a1)

-- Equality of subarrays except for two positions that have the elements exchanged
exchange::Eq a => Array a -> Array a -> Int -> Int -> Int -> Int -> Bool
exchange a1 a2 l u i j =  if l>=(snd a1) || l>=(snd a2) then throw (IndexOutOfRange l) else 
   if u>(snd a1) || u>(snd a2) then throw (IndexOutOfRange u) else if i<l || i>=u then throw (IndexOutOfRange i) else 
   if j<l || j>=u then throw (IndexOutOfRange j) else exchange_aux l1 l2 i j (get a1 i) (get a2 i)
   where
      l1 = L.take (u-l) (L.drop l (M.toAscList (fst a1)))
      l2 = L.take (u-l) (L.drop l (M.toAscList (fst a2)))
      exchange_aux [] []  _ _ _ _ = True
      exchange_aux ((k,s1):ss1) ((_,s2):ss2) i j x1 x2
         |k==i = exchange_aux ss1 ss2 i j x1 x2
         |k==j = (s1==x2) && (s2==x1) && exchange_aux ss1 ss2 i j x1 x2
         |otherwise = (s1==s2) && exchange_aux ss1 ss2 i j x1 x2   

-- Sum of the elements of an integer's subarray
sum::Array Int -> Int -> Int -> Int
sum a l u = if l>=(snd a) then throw (IndexOutOfRange l) else 
   if u>(snd a) then throw (IndexOutOfRange u) else M.foldrWithKey f 0 (fst a)
      where f k a b = if (l<=k) && (k<u) then (a+b) else b

-- Number of occurrences of an element in the subarray
numof::Eq a => Array a -> a -> Int -> Int -> Int
numof a x l u = if l>=(snd a) then throw (IndexOutOfRange l) else 
   if u>(snd a) then throw (IndexOutOfRange u) else M.foldrWithKey f 0 (fst a)
      where f k a b = if (l<=k) && (k<u) && (a==x) then (b+1) else b

-- Permutation of subarrays
permut::Ord a => Array a -> Array a -> Int -> Int -> Bool
permut a1 a2 l u = if l>=(snd a1) || l>=(snd a2) then throw (IndexOutOfRange l) else 
   if u>(snd a1) || u>(snd a2) then throw (IndexOutOfRange u) else (snd a1) == (snd a2) && l1 == l2
      where l1 = L.sort (L.take (u-l) (L.drop l (M.elems (fst a1))))
            l2 = L.sort (L.take (u-l) (L.drop l (M.elems (fst a2))))

-- Permutation of subarrays and equality of the rest of the arrays
permut_sub::Ord a => Array a -> Array a -> Int -> Int -> Bool
permut_sub a1 a2 l u = if l>=(snd a1) || l>=(snd a2) then throw (IndexOutOfRange l) else 
   if u>(snd a1) || u>(snd a2) then throw (IndexOutOfRange u) else (L.take l l1) == (L.take l l2) &&
   (L.drop u l1) == (L.drop u l2) && (L.sort (L.take (u-l) (L.drop l l1))) == (L.sort (L.take (u-l) (L.drop l l2)))
       where l1 = M.elems (fst a1)
             l2 = M.elems (fst a2)

-- Permutation of arrays
permut_all::Ord a => Array a -> Array a -> Bool
permut_all a1 a2 = (snd a1) == (snd a2) && permut a1 a2 0 (snd a1)