{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}

module Sized
    ( Allv(..)
      , Sized(..)
    ) where

import GHC.Generics
import qualified Data.Set as S
import qualified Data.PQueue.Min as Q
import System.IO.Unsafe (unsafePerformIO)
import TemplateAllv

--Class Allv and its instances
class Allv a where
  allv :: [a]

instance Allv Int where
  allv = [1..100]

instance Allv Char where
  allv = ['a'..'z']

{-data Exp = Const Int | Var Char | Sum Exp Exp | Prod Exp Exp
   deriving (Generic, Show)
instance Sized Exp where

instance Allv Exp where
  allv = map Const allv
        ++ map Var allv
        ++ map f (compose allv allv)
        ++ map g (compose allv allv)
        where f (x,y) = Sum x y
              g (u,v) = Prod u v

data List a = Nil | Cons a (List a)
   deriving (Generic, Show)
instance Sized a => Sized (List a) where

instance Allv a => Allv (List a) where
  allv = [Nil]
         ++ map f (compose allv allv)
         where f (x,xs) = Cons x xs

data Tree a = Empty | Node (Tree a) a (Tree a)
   deriving (Generic, Show)
instance Sized a => Sized (Tree a) where

instance Allv a => Allv (Tree a) where
  allv = [Empty]
         ++ map f (compose allv (compose allv allv))
         where f (t1,(x,t2)) = Node t1 x t2-}



-- | This is the exported, visible class that inherits from Allv.
class (Allv a) => Sized a where
  -- This function returns the first n elements of size m from the "allv" list
  sized::Int->Int->[a]
  sized n m = take n (filter (\x-> (size x) == m) allv)

  --This function takes an integer n and returns the n first elements of the "allv" list
  smallest::Int->[a]
  smallest n = take n allv

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

-- | For basic types we must give the instances
instance Sized Int where
  size x = 1

instance Sized Char where
  size x = 1


-----------------------------------------------------------------------------------
------------------------compose of 2 lists-----------------------------------------
-----------------------------------------------------------------------------------


---------------------solo para depuracion del compose--------------------------------

traza :: Show a => String -> a -> b -> b

traza cadena valor exp = unsafePerformIO $ do putStrLn cadena
                                              print valor
                                              return exp          
-----------------------------------------------------------------------------------------

newtype QElem a b = QE (a,(Int,Int),b)
        deriving Show

instance Eq a => Eq (QElem a b) where
   QE (x,_,_) == QE (y,_,_) = x == y


-- In the total order, it is important that ties are won by the smaller diagonal

instance Ord a => Ord (QElem a b) where
   QE (x,(i,j),_) <= QE (x',(i',j'),_) = x < x' || (x == x' && i + j <= i' + j')