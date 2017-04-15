{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}

module Arbitrary where

import GHC.Generics

-- | This is the exported, visible class
class Arbitrary a where
    randList :: Int -> [a]

{-instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary = sized (arbHeap Nothing)
   where
    arbHeap mx n =
      frequency $
        [ (1, return Empty) ] ++
        [ (7, do my <- arbitrary `suchThatMaybe` ((>= mx) . Just)
                 case my of
                   Nothing -> return Empty
                   Just y  -> liftM2 (Node y) arbHeap2 arbHeap2
                    where arbHeap2 = arbHeap (Just y) (n `div` 2))
        | n > 0
        ]-}
