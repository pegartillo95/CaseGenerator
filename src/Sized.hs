{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}

module Sized
    ( Sized(..)
      ,Tree(..)
--      , alltree
      , compose
      , diags
      , Color
    ) where

import GHC.Generics
import qualified Data.Set as S
import qualified Data.PQueue.Min as Q
import System.IO.Unsafe (unsafePerformIO)


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
  allv   =[1..5]

instance Sized Char where
  size x = 1
  allv   = ['a'..'z']

instance Sized a => Sized [a] where


-------------------------------- examples -------------------------------------------


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


data Color = R | G | C Int Color
             deriving (Generic, Show)
instance Sized Color where



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

-----------------------------------------------------------------------------------------
-- It produces an increasing sorted list from a partially sorted list of values
-- coming from a lattice of pairs which has been traversed by diagonalization of 
-- the cartesian product of two increasing lists and a monotonic operator
--
-- The nice thing is that it works for two infinite input lists
-----------------------------------------------------------------------------------------


compose ::(GSized f, GSized g) => [f a] -> [g a] -> [(f :*: g) a]

compose xs ys = --reorder lattice (S.singleton (0,0)) (Q.singleton e)
                map (\(QE (s,p,v)) -> v) (e:lattice)
  where e:lattice = concat $ diags 0 0 0 xs ys

--
-- It builds the lattice of tuples from two lists, each one may be either
-- finite or infinite


diags :: (GSized f, GSized g) => 
         Int -> Int -> Int -> [f a] -> [g a] -> [[QElem Int ((f :*: g) a)]]

diags _ _  _  [] [] = [[]]
diags i dx dy xs ys
    | fullDiag     = [QE (tup k) | k <- [0..i]] : diags (i+1) dx dy xs ys
    | finiteFirst  = diags (i-1) dx     (dy+1) xs  ysr
    | finiteSecond = diags (i-1) (dx+1) dy     xsr ys
    | otherwise    = diags (i-2) (dx+1) (dy+1) xsr ysr

  where xs'          = drop i xs
        ys'          = drop i ys
        xsr          = tail xs
        ysr          = tail ys
        fullDiag     = not (null xs') && not (null ys')
        finiteFirst  = null xs' && not (null ys')
        finiteSecond = not (null xs') && null ys'
        tup k        = (gsize x + gsize y, (k+dx, i-k+dy), x:*:y)
                       where x = xs !! k 
                             y = ys !! (i-k)  


{-reorder :: (Ord a) => 
           [QElem a b] -> S.Set (Int,Int) -> Q.MinQueue (QElem a b) 
                       -> [b]
reorder [] _   queue = map (\(QE (s,p,v)) -> v) $ Q.toList queue
reorder xs set queue = v : reorder xs3 s3 q3

  where QE (s,(i,j),v) = Q.findMin queue
        queue'         = Q.deleteMin queue
        set'           = S.delete (i,j) set
        (s2,q2,xs2)    = update (i,j+1) set' queue' xs
        (s3,q3,xs3)    = update (i+1,j) s2 q2 xs2


update :: (Ord a) => 
          (Int,Int) -> S.Set (Int,Int) -> Q.MinQueue (QElem a b) -> 
          [QElem a b] -> (S.Set (Int,Int), Q.MinQueue (QElem a b), [QElem a b])

update (i,j) set q xs = case (i,j) `S.member` set of
                          False -> case z of
                                     Nothing   -> (set,q,xs)
                                     Just elem -> (S.insert (i,j) set,
                                                   Q.insert elem q,
                                                   xsWithoutElem)
                          True  -> (set,q,xs)

   where (z, xsWithoutElem) = remove (i,j) xs


-- 
-- It extracts an element from the lattice of tuples. If a tuple located further than 
-- the tuple looked for is found, then Nothing is returned
--

remove ::(Int,Int) -> [QElem a b] -> (Maybe (QElem a b), [QElem a b])

remove p [] = error ("Tuple "++ show p ++ " not in list")
remove p@(i,j) (x@(QE (s,p'@(i',j'),v)) : xs) 
         | p == p'      = (Just $ QE (s,p,v), xs)
         | notExists    = (Nothing, x:xs) 
         | otherwise    = (z, x : xs')

   where (z, xs')  = remove p xs 
         notExists = i'+j' > i+j || (i'+j' == i+j && j' < j) || null xs-}