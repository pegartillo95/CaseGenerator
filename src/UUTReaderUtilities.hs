{-# LANGUAGE TemplateHaskell #-}

module UUTReaderUtilities where

import Language.Haskell.TH
import TemplateAllv

listVar :: Int -> [Name]
listVar 0 = []
listVar n = (mkName ("x"++ show n)):(listVar (n-1))

tupleParam :: [Name] -> [PatQ]
tupleParam listOfVar = [tupP (map varP listOfVar)]

body :: String -> [Name] -> ExpQ
body f_name listOfVar = (lookupValueName f_name >>=
                          (\(Just name)->(appsE ((varE name):(map varE listOfVar)))
	                        ))
	                       

body2 :: String -> [Name]-> ExpQ
body2 f_name listOfVar = (lookupValueName f_name >>=
                          (\(Just name)->(appsE ((varE name):((map varE listOfVar)++[varE nameY])))
	                        ))

nameY :: Name
nameY = mkName "y"