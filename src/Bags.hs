module Bags where 

import qualified Data.Map.Lazy as M
import System.Random
import System.IO.Unsafe

type Bag a = M.Map a Int

-- Number of occurrences of an element
nb_occ::Ord a => a -> Bag a -> Int
nb_occ x b = M.findWithDefault 0 x b

-- Membership in the multiset
mem::Ord a => a -> Bag a -> Bool
mem x b = M.member x b

-- Equality of multisets
eq_bag::Ord a => Bag a -> Bag a -> Bool
eq_bag b1 b2 = M.isSubmapOf b1 b2 && M.isSubmapOf b2 b1

-- Construction of empty multiset
empty_bag::Bag a
empty_bag = M.empty

-- Construction of single-element multiset
singleton::a -> Bag a
singleton x = M.singleton x 1

-- Multisets union
union::Ord a => Bag a -> Bag a -> Bag a
union b1 b2 = M.unionWith (+) b1 b2

-- Add an element
add::Ord a => a -> Bag a -> Bag a
add x b = M.insertWith (+) x 1 b

-- Multiset size
card::Bag a -> Int
card b = M.foldr (+) 0 b

-- Multisets difference
diff::Ord a => Bag a -> Bag a -> Bag a
diff b1 b2 = M.differenceWith f b1 b2
              where f x y = if x<=y then Nothing else Just (x-y) 

-- Random element of the multiset
choose::Bag a -> a
choose b = unsafePerformIO f
            where f = do
                        newStdGen
                        g <- getStdGen
                        return (fst (M.elemAt (fst (randomR (0,(M.size b)-1) g)) b))