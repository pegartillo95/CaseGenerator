module UUT where

uutNargs ::Int
uutNargs = 2

uutMethods :: [String]
uutMethods = ["uutPrec", "uutMethod", "uutPost"]

uutPrec :: Int -> Int -> Bool
uutPrec x y = True

uutMethod :: Int -> Int -> Int
uutMethod x y = x+y

uutPost :: Int -> Int -> Int -> Bool
uutPost x y z = True

f x y = x <= y

g :: Ord a => a -> a -> Bool
g = \x y -> x <= y