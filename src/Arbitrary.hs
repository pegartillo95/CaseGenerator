{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}

module Arbitrary where

import GHC.Generics

-- | This is the exported, visible class
--class Arbitrary a where
--	randList :: Int -> [a]
--	default randList :: (Generic a, GArbitrary (Rep a)) => Int -> a
--	randList n = map to (grandList n)


-- | This is the generic, non-visible class
--class GArbitrary f where
--	grandList :: Int -> [f a]

-- | Unit: used for constructors without arguments
--instance GArbitrary U1 where


-- | Products: encode multiple arguments to constructors
--instance (GArbitrary a, GArbitrary b) => GArbitrary (a :*: b) where


-- | Sums: encode choice between constructors
--instance (GArbitrary a, GArbitrary b) => GArbitrary (a :+: b) where


-- | Meta-information (constructor names, etc.)
--instance (GArbitrary f) => GArbitrary (M1 i c f) where


-- | Constants, additional parameters and recursion of kind *
--instance Arbitrary a => GArbitrary (K1 i a) where