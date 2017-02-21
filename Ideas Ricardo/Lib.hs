module Lib
    ( compose
    , compon
    ) where

import qualified Data.Set as S
import qualified Data.PQueue.Min as Q
import System.IO.Unsafe (unsafePerformIO)

---------------------solo para depuracion--------------------------------

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

compose :: (Num a, Ord a, Show a) => [a] -> [a] -> [(a,(Int,Int),(a,a))]

compose xs ys = reorder lattice (S.singleton (0,0)) (Q.singleton e)

  where e:lattice = concat $ diags 0 0 0 xs ys       


reorder :: (Show a,Show b,Ord a) => 
           [QElem a b] -> S.Set (Int,Int) -> Q.MinQueue (QElem a b) 
                       -> [(a,(Int,Int),b)]
reorder [] _   queue = map (\(QE (s,p,v)) -> (s,p,v)) $ Q.toList queue
reorder xs set queue = (s,(i,j),v) : reorder xs3 s3 q3

  where QE (s,(i,j),v) = Q.findMin queue
        queue'         = Q.deleteMin queue
        set'           = S.delete (i,j) set
        (s2,q2,xs2)    = update (i,j+1) set' queue' xs
        (s3,q3,xs3)    = update (i+1,j) s2 q2 xs2


update :: (Show a,Show b,Ord a) => 
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

remove :: (Show a, Show b) => 
          (Int,Int) -> [QElem a b] -> (Maybe (QElem a b), [QElem a b])

remove p [] = error ("Tuple "++ show p ++ " not in list")
remove p@(i,j) (x@(QE (s,p'@(i',j'),v)) : xs) 
         | p == p'      = --traza "###### found p " p $ 
                          (Just $ QE (s,p,v), xs)
         | notExists    = --traza "###### remove: Nothing" p' $ 
                          (Nothing, x:xs) 
         | otherwise    = --traza "###### remove: p ########### " p $ 
                          (z, x : xs')

   where (z, xs')  = remove p xs 
         notExists = i'+j' > i+j || (i'+j' == i+j && j' < j) || null xs


--
-- It builds the lattice of tuples from two lists, each one may be either
-- finite or infinite
--

compon  :: (Num a, Ord a, Show a) => [a] -> [a] -> [QElem a (a,a)]
compon xs ys = concat $ diags 0 0 0 xs ys


diags :: (Num a, Ord a, Show a) => 
         Int -> Int -> Int -> [a] -> [a] -> [[QElem a (a,a)]]

diags _ _  _  [] [] = [[]]
diags i dx dy xs ys
    | fullDiag     = [QE (tup k) | k <- [0..i]] : diags (i+1) dx dy xs ys
    | finiteFirst  = --traza "FiniteFirst " i $ 
                     diags (i-1) dx     (dy+1) xs  ysr
    | finiteSecond = --traza "FiniteSecond " i $ 
                     diags (i-1) (dx+1) dy     xsr ys
    | otherwise    = diags (i-2) (dx+1) (dy+1) xsr ysr

  where xs'          = drop i xs
        ys'          = drop i ys
        xsr          = tail xs
        ysr          = tail ys
        fullDiag     = not (null xs') && not (null ys')
        finiteFirst  = null xs' && not (null ys')
        finiteSecond = not (null xs') && null ys'
        tup k        = (x+y, (k+dx, i-k+dy), (x,y))
                       where x = xs !! k 
                             y = ys !! (i-k)



















