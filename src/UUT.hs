module UUT where

uutNargs ::Int
uutNargs = 2

uutTypesStr :: [String]
uutTypesStr = ["Int", "Int"]

uutMethods :: [String]
uutMethods = ["uutPrec", "uutMethod", "uutPost"]

uutPrec :: Int -> Int -> Bool
uutPrec x y = True

uutMethod :: Int -> Int -> Int
uutMethod x y = x+y

uutPost :: Int -> Int -> Int -> Bool
uutPost x y z = True