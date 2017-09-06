{-# LANGUAGE DeriveGeneric #-}
module UUT where


import qualified Arrays as A
import qualified Bags as B
import qualified Sets as S
import qualified Sequences as Q
import Assertion

import GHC.Generics

-- Searching in a Binary Search tree
-- This is an example where the user defines a new type

data Tree a = Node (Tree a) a (Tree a)
            | Empty
            deriving Generic


uutNargs :: Int
uutNargs = 2

uutMethods :: [String]
uutMethods = ["uutPrec", "uutMethod", "uutPost"]

uutName :: String
uutName = "insertBST" 


uutPrec :: Int -> Tree Int -> Bool
uutPrec x t = sorted $ inorder t



inorder Empty        = []
inorder (Node l x r) = inorder l ++ (x : inorder r)

sorted []       = True
sorted [x]      = True
sorted (x:y:xs) = x <= y && sorted (y:xs)


uutMethod :: Ord a => a -> Tree a -> Bool
uutMethod x Empty = False
uutMethod x t@(Node l y r)
  | x < y  = uutMethod x l
  | x == y = True
  | x > y  = uutMethod x l -- error, debería ser r

uutPost x t o = o == (x `elem` inorder t)




