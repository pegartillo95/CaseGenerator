module UUT where

import TemplateAllv

uutName::String
uutName = "compose"

uutNargs ::Int
uutNargs = 2

uutPrec :: [a] -> [b] -> Bool
uutPrec xs ys = True

uutMethod :: [a] -> [b] -> [(a,b)]
uutMethod xs ys = compose xs ys

uutPost :: [a] -> [b] -> [(a,b)] -> Bool
uutPost xs ys z = True

nombres:: [String]
nombres = ["pp1", "pp2"]