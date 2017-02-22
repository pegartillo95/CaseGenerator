

{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}
 

module Main where

import GHC.Generics
import Compose as C

-----------------------------------------------------------------------------------
-- | Programming generic size and generic enumeration of values
-- | autor: Ricardo Peña & Pedro García Castillo, febrero 2017
-----------------------------------------------------------------------------------

--Dumb main function
main :: IO ()
main = putStrLn "Test suite not yet implemented"


-- | This is the exported, visible class
class Sized a where
  size ::  a -> Int
  allv :: [a]
  default size :: (Generic a, GSized (Rep a)) => a -> Int
  size a = gsize (from a)
  default allv :: (Generic a, GSized (Rep a)) => [a]
  allv   = map to gall

-- | This is the generic, non-visible class
class GSized f where
  gsize :: f a -> Int
  gall  :: [f a]

-- | Unit: used for constructors without arguments
instance GSized U1 where
  gsize U1 = 0
  gall     = [U1]

-- | Products: encode multiple arguments to constructors
instance (GSized a, GSized b) => GSized (a :*: b) where
  gsize (x :*: y) = gsize x + gsize y
  gall = compose gall gall
           where compose xs ys = concat [diag i xs ys | i <- [0..]]
                 diag i xs ys = if(null(drop i xs)) 
                                   then if(null(drop i ys))
                                           then if(firstLongest xs ys)
                                                   then if(i < (length xs))
                                                         then [(xs !! k) :*: (ys !! (i-k)) | k <- [(i-(length ys)+1)..((length xs)-1)]]
                                                         else diag (mod i (length xs)) xs ys
                                                   else if(i < (length ys))
                                                         then [(xs !! k) :*: (ys !! (i-k)) | k <- [(i-(length ys)+1)..((length xs)-1)]]
                                                         else diag (mod i (length ys)) xs ys
                                           else [(xs !! (k-(i-(length xs) + 1))) :*: (ys !! (i-k)) | k <-[(i-(length xs) +1)..i]]
                                    else if(null(drop i ys))
                                           then [(xs !! k) :*: (ys !! (i-k)) | k <- [(i-(length ys) +1)..i]]
                                           else [(xs !! k) :*: (ys !! (i-k)) | k <- [0..i]]
                 
                 firstLongest xs ys = if(length xs >= length ys) 
                                       then True
                                       else False


-- | Sums: encode choice between constructors
instance (GSized a, GSized b) => GSized (a :+: b) where
  gsize (L1 x) = gsize x
  gsize (R1 x) = gsize x
  gall         = map L1 gall ++ map R1 gall

-- | Meta-information (constructor names, etc.)
instance (GSized f) => GSized (M1 i c f) where
  gsize (M1 x) = gsize x
  gall         = map M1 gall

-- | Constants, additional parameters and recursion of kind *
instance Sized a => GSized (K1 i a) where
  gsize (K1 x) = size x
  gall         = map K1 allv

-- | For basic types we must give the instances
instance Sized Int where
  size x = 1
  allv   =[1..100]

instance Sized Char where
  size x = 1
  allv   = ['a'..'z']

instance Sized a => Sized [a] where


--------------------------------ejemplos-------------------------------------------


data Tree a = E | N a (Tree a) (Tree a)
              deriving (Generic, Show)
instance Sized a => Sized (Tree a) where

alltree :: Int -> [Tree Int]
alltree n = E: map node (compose [0..n] (compose (alltree n) (alltree n)))
            where compose xs ys = concat [diag i xs ys | i <- [0..n]]
                  diag i xs ys  = [((xs !! k) , (ys !! (i-k))) | k <- [0..i]]
                  node (i,(t1,t2))  = N i t1 t2

data NTree a = NT a Int [NTree a]
               deriving (Generic, Show)
instance Sized a => Sized (NTree a) where