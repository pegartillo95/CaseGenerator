module UUT where

import TemplateAllv

uutNargs ::Int
uutNargs = 2

uutTypesStr :: [String]
uutTypesStr = ["[Int]", "[Int]"]

uutMethods :: [String]
uutMethods = ["uutPrec", "uutMethod", "uutPost"]

uutPrec :: [a] -> [b] -> Bool
uutPrec xs ys = True

uutMethod :: [a] -> [b] -> [(a,b)]
uutMethod xs ys = compose xs ys

uutPost :: [a] -> [b] -> [(a,b)] -> Bool
uutPost xs ys z = True