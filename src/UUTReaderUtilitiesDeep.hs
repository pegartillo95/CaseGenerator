{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module UUTReaderUtilitiesDeep where

import Control.Monad
import Language.Haskell.TH
import UUT
import Sized

--------------Create a list of variables -----------------------------
listVar :: Int -> [Name]
listVar 0 = []
listVar n = (mkName ("x"++ show n)):(listVar (n-1))

-------------Linked tuples creator-------------------------------------

tupleP :: Int -> PatQ
tupleP n = tupP (listOfPs n)

listOfPs :: Int -> [PatQ]
listOfPs 0 = []
listOfPs n = (varP $ mkName ("x" ++ (show n))):(listOfPs (n-1))