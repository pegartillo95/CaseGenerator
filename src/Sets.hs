{-# LANGUAGE DeriveDataTypeable #-}

module Sets where

import qualified Data.Set as S
import System.Random
import System.IO.Unsafe
import Control.Exception
import Data.Typeable

type Set a = S.Set a

data SetsException = EmptySet | IndexOutOfRange Int
   deriving (Show, Typeable)
instance Exception SetsException

-- Membership in the set
mem::Ord a => a -> Set a -> Bool
mem x s = S.member x s

-- Sets equality
(==)::Ord a => Set a -> Set a -> Bool
(==) s1 s2 = S.isSubsetOf s1 s2 && S.isSubsetOf s2 s1

-- Subset
subset::Ord a => Set a -> Set a -> Bool
subset = S.isSubsetOf

-- Emptiness test
is_empty::Set a -> Bool
is_empty = S.null

-- Add an element
add::Ord a => a -> Set a -> Set a
add = S.insert

-- Construction of a single-element set
singleton::a -> Set a
singleton = S.singleton

-- Remove an element
remove::Ord a => a -> Set a -> Set a
remove = S.delete

-- Sets union
union::Ord a => Set a -> Set a -> Set a
union = S.union

-- Sets intersection
inter::Ord a => Set a -> Set a -> Set a
inter = S.intersection

-- Sets difference
diff::Ord a => Set a -> Set a -> Set a
diff = S.difference

-- Filter over a set
filter::(a -> Bool) -> Set a -> Set a
filter = S.filter

-- Map of a set
map::Ord b =>(a -> b) -> Set a -> Set b
map = S.map

-- Size of a set
cardinal::Set a -> Int
cardinal = S.size

-- Nth element of a set
nth::Int -> Set a -> a
nth i s = if (i<0 || i>=(cardinal s)) then throw (IndexOutOfRange i) else S.elemAt i s

-- Minimum element of an integer's set
min_elt::Set Int -> Int
min_elt s = if (is_empty s) then throw EmptySet else (S.findMin s)

-- Maximum element of an integer's set
max_elt::Set Int -> Int
max_elt s = if (is_empty s) then throw EmptySet else (S.findMax s)

-- Set of an integer's interval
interval::Int -> Int -> Set Int
interval i j = S.fromAscList [i..(j-1)]

-- Sum of the integers obtained by applying a function to the set
sum::Set a -> (a -> Int) -> Int
sum s f = S.foldr (+) 0 (S.map f s)

-- Random element of the set
choose::Set a -> a
choose s = unsafePerformIO f
            where f = do
                        newStdGen
                        g <- getStdGen
                        return (S.elemAt (fst (randomR (0,(cardinal s)-1) g)) s)