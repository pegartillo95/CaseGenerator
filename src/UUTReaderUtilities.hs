{-# LANGUAGE TemplateHaskell #-}

module UUTReaderUtilities where

import Language.Haskell.TH

listVar :: Int -> [Name]
listVar 0 = []
listVar n = (mkName ("x"++ show n)):(listVar (n-1))

tupleParam :: [Name] -> [PatQ]
tupleParam listOfVar = [tupP (map varP listOfVar)]

body :: [Name] -> ExpQ
body listOfVar = (appsE (map varE listOfVar))