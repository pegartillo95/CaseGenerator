{-# LANGUAGE TemplateHaskell #-}

module UUTReaderUtilities where

import Language.Haskell.TH

listVar :: Int -> [Name]
listVar 0 = []
listVar n = (mkName ("x"++ show $n)):(listVar ($n-1))

tupleParam :: [Name] -> [PatQ]
tupleParam listOfVar = [tupP (map varP listOfVar)]

body :: Name -> [Name] -> ExpQ
body f_name listOfVar = (appsE ((varE f_name):(map varE listOfVar)))

body2 :: Name -> [Name]-> ExpQ
body2 f_name listOfVar = (appsE ((varE f_name):((map varE listOfVar)++[varE nameY])))

nameY :: Name
nameY = mkName "y"