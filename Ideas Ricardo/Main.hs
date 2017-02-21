

{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}
 

module Generator where

import System.IO.Unsafe (unsafePerformIO)

import GHC.Generics
---------------------solo para depuracion--------------------------------

traza :: Show a => String -> a -> b -> b

traza cadena valor exp = unsafePerformIO $ do putStrLn cadena
                                              print valor
                                              return exp          

-----------------------------------------------------------------------------------
-- | Programming generic size and generic enumeration of values
-- | autor: Ricardo Peña & Pedro García Castillo, febrero 2017
-----------------------------------------------------------------------------------

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


         {-where compose xs ys = diag 0 xs ys False False
               diag _ [] [] _ _ = []
               diag i xs ys a b
                  | a && b = zs ++ diag (i-1) xs' ys' True True
                  | a && (not b) = if (null(drop (i+1) ys))
                                    then zs  ++ diag (i-1) xs' ys' True True
                                    else zs ++ diag i xs ys' True False
                  | (not a) && b = if (null(drop (i+1) xs))
                                    then zs ++ diag (i-1) xs' ys' True True
                                    else zs ++ diag i xs' ys False True
                  | (not a) && (not b) = if (null(drop (i+1) xs) && null(drop (i+1) ys))
                                        then zs ++ diag (i-1) xs' ys' True True
                                        else if (null(drop (i+1) xs) && not(null(drop (i+1) ys)))
                                                then zs ++ diag i xs ys' True False
                                                else if (not(null(drop (i+1) xs)) && null(drop (i+1) ys))
                                                        then zs ++ diag i xs' ys False True
                                                        else zs ++ diag (i+1) xs ys False False
                  where xs' = drop 1 xs
                        ys' = drop 1 ys
                        zs = [(xs !! k) :*: (ys !! (i-k)) | k <- [0..i]]-}


-- | Sums: encode choice between constructors
instance (GSized a, GSized b) => GSized (a :+: b) where
  gsize (L1 x) = gsize x
  gsize (R1 x) = gsize x
  gall         = map L1 gall ++ map R1 gall

-- | Meta-information (constructor names, etc.)
instance (GSized f) => GSized (M1 i c f) where
  gsize (M1 x) = gsize x
  gall         = map M1 gall

-- | Parameter occurrences for kind *->* count as 1
--instance  GSized Par1 where
--  gsize (Par1 x) = 1

-- | Recursion over values of type (f a) for f: *->*
--instance (Sized f) => GSized (Rec1 f) where
--  gsize (Rec1 x) = size x

-- | Parameter occurrences for kind *
-- instance Sized a => GSized (K1 P a) where
--   gsize (K1 x) = 1
--   gall         = map K1 allv

-- | Recursion of kind *
-- instance Sized a => GSized (K1 R a) where
--    gsize (K1 x) = size x
--    gall         = map K1 allv

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

--New datatypes just for some more testing
data IntChar = IN Int Char
              deriving(Generic,Show)
instance Sized IntChar where

data TripleInt = TrIn Int Int Int
              deriving(Generic,Show)
instance Sized TripleInt where

data CharOrInt = Char Char | Int Int
              deriving(Generic,Show)
instance Sized CharOrInt where





compon  :: [a] -> [b] -> [(a,b)]
compon xs ys = concat $ diags 0 xs ys


diags :: Int -> [a] -> [b] -> [[(a,b)]]
diags i [] [] = [[]]

diags i xs ys
    | fullDiag     = [(xs !! k, ys !! (i-k)) | k <- [0..i]] : diags (i+1) xs ys
    | finiteFirst  = --traza "FiniteFirst " i $ 
                     diags (i-1) xs  ysr
    | finiteSecond = --traza "FiniteSecond " i $ 
                     diags (i-1) xsr ys
    | otherwise    = diags (i-2) xsr ysr

  where xs'          = drop i xs
        ys'          = drop i ys
        xsr          = tail xs
        ysr          = tail ys
        fullDiag     = not (null xs') && not (null ys')
        finiteFirst  = null xs' && not (null ys')
        finiteSecond = not (null xs') && null ys'







