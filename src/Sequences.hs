{-# LANGUAGE DeriveDataTypeable #-}

module Sequences where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Sets as Sets
import Control.Exception
import Data.Typeable

type Seq a = [a]

data SeqsException = IndexOutOfRange Int
   deriving (Show, Typeable)
instance Exception SeqsException

-- Sequence's length
length::Seq a -> Int
length = L.length

-- Ask for the element in a position
get::Seq a -> Int -> a
get s i = if i>=(Sequences.length s) then throw (IndexOutOfRange i) else s L.!! i

-- Set an element in a position
set::Seq a -> Int -> a -> Seq a
set s i v = if i>(Sequences.length s) then throw (IndexOutOfRange i) else (L.take i s) L.++ [v] L.++ (L.drop i s)

-- Sequences equality
(==)::Eq a => Seq a -> Seq a -> Bool
(==) s1 s2 = (s1 Prelude.== s2)

-- Add an element in the sequence's head
cons::a -> Seq a -> Seq a
cons v s = v:s

-- Add an element in the end
snoc::a -> Seq a -> Seq a
snoc v s = s L.++ [v]

-- Subsequence between the given positions
subsec::Seq a -> Int -> Int -> Seq a
subsec s i j = if i>=(Sequences.length s) then throw (IndexOutOfRange i) else 
   if j>(Sequences.length s) then throw (IndexOutOfRange j) else L.take (j-i) (L.drop i s)

-- Subsequence starting in the given position
subsec_ini::Seq a -> Int -> Seq a
subsec_ini s i = if i>=(Sequences.length s) then throw (IndexOutOfRange i) else L.drop i s 

-- Subsequence finishing in the given position
subsec_fin::Seq a -> Int -> Seq a
subsec_fin s i = if i>=(Sequences.length s) then throw (IndexOutOfRange i) else L.take (i-1) s

-- Concatenation
(++)::Seq a -> Seq a -> Seq a
(++) s1 s2 = s1 L.++ s2

-- Create a sequence from a function
create::Int -> (Int -> a) -> Seq a
create n f = L.map f (L.take n [0,1..])

-- Membership in the sequence
mem::Eq a => a -> Seq a -> Bool
mem = L.elem

-- True if all the elements are different
distinct::Ord a => Seq a -> Bool
distinct = distinct_aux.(L.sort)
   where
      distinct_aux []  = True
      distinct_aux [x] = True
      distinct_aux (x1:x2:xs) = x1<x2 && distinct_aux (x2:xs)

-- Reversed sequence
reverse::Seq a -> Seq a
reverse = L.reverse

-- Set of the sequence's elements
to_set::Ord a => Seq a -> Sets.Set a
to_set = L.foldr Sets.add S.empty

-- True if the subsequence is sorted (in ascending order)
sorted_sub::Ord a => Seq a -> Int -> Int -> Bool
sorted_sub s l u = if l>=(Sequences.length s) then throw (IndexOutOfRange l) else 
   if u>(Sequences.length s) then throw (IndexOutOfRange u) else sorted (L.take (u-l) (L.drop l s))

-- True if the sequence is sorted (in ascending order)
sorted::Ord a => Seq a -> Bool
sorted []  = True
sorted [x] = True
sorted (x1:x2:xs) = x1<=x2 && sorted (x2:xs)

-- Number of occurrences of an element in the subsequence
occ::Eq a => a -> Seq a -> Int -> Int -> Int
occ x s l u = if l>=(Sequences.length s) then throw (IndexOutOfRange l) else 
   if u>(Sequences.length s) then throw (IndexOutOfRange u) else L.foldr f 0 (L.take (u-l) (L.drop l s))
               where f y acc = if x Prelude.==y then (1+acc) else acc

-- Subsequences equality
seq_eq_sub::Eq a => Seq a -> Seq a -> Int -> Int -> Bool
seq_eq_sub s1 s2 l u = if l>=(Sequences.length s1) || l>=(Sequences.length s2) then throw (IndexOutOfRange l) else 
   if u>(Sequences.length s1) || u>(Sequences.length s2) then throw (IndexOutOfRange u) else 
      Sequences.length s1 Prelude.== Sequences.length s2 && (L.take (u-l) (L.drop l s1)) Prelude.== (L.take (u-l) (L.drop l s2))

-- Equality of sequences except for two positions that have the elements exchanged
exchange::Eq a => Seq a -> Seq a -> Int -> Int -> Bool
exchange s1 s2 i j = if i>=(Sequences.length s1) || i>=(Sequences.length s2) then throw (IndexOutOfRange i) else 
   if j>=(Sequences.length s1) || j>=(Sequences.length s2) then throw (IndexOutOfRange j) else 
   exchange_aux s1 s2 i j 0 (get s1 i) (get s2 i)
      where 
         exchange_aux [] [] _ _ _ _ _ = True
         exchange_aux [] _  _ _ _ _ _ = False
         exchange_aux _ []  _ _ _ _ _ = False
         exchange_aux (s1:ss1) (s2:ss2) i j pos x1 x2
            |pos Prelude.==i = exchange_aux ss1 ss2 i j (pos+1) x1 x2
            |pos Prelude.==j = (s1 Prelude.==x2) && (s2 Prelude.==x1) && exchange_aux ss1 ss2 i j (pos+1) x1 x2
            |otherwise = (s1 Prelude.==s2) && exchange_aux ss1 ss2 i j (pos+1) x1 x2

-- Permutation of subsequences
permut::Ord a => Seq a -> Seq a -> Int -> Int -> Bool
permut s1 s2 l u =  if l>=(Sequences.length s1) || l>=(Sequences.length s2) then throw (IndexOutOfRange l) else 
   if u>(Sequences.length s1) || u>(Sequences.length s2) then throw (IndexOutOfRange u) else (L.length s1 Prelude.== L.length s2) && 
   (L.sort (L.take (u-l) (L.drop l s1))) Prelude.== (L.sort (L.take (u-l) (L.drop l s2)))

-- Permutation of subsequences and equality of the rest of the sequences
permut_sub::Ord a => Seq a -> Seq a -> Int -> Int -> Bool
permut_sub s1 s2 l u =  if l>=(Sequences.length s1) || l>=(Sequences.length s2) then throw (IndexOutOfRange l) else 
   if u>(Sequences.length s1) || u>(Sequences.length s2) then throw (IndexOutOfRange u) else (L.take l s1 Prelude.== L.take l s2) && 
   (L.drop (u-l) s1 Prelude.== L.drop (u-l) s2) && (L.sort (L.take (u-l) (drop l s1))) Prelude.== (L.sort (L.take (u-l) (drop l s2)))

-- Permutation of sequences
permut_all::Ord a => Seq a -> Seq a -> Bool
permut_all s1 s2 = permut s1 s2 0 (L.length s1)