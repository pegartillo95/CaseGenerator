{-# LANGUAGE TemplateHaskell, ScopedTypeVariables#-}

module UUTReader where

import Control.Monad
import Data.String.Utils
import Language.Haskell.TH
import GHC.Generics
import Sized
import Arbitrary
import TemplateAllv
import TemplateArbitrary
import UUT
import UUTReaderUtilities

instance Sized T

test_UUT :: [Bool]
test_UUT = test

-------------call to gen_all and gen_arbitrary -------------------------------------
$(gen_allv_str_listQ (notDefTypesQMonad (get_f_inp_types (head uutMethods))))

--------------Printing the ending information---------------------------------------

printInfo :: [Bool] -> String
printInfo xs = "Tried " ++ show (length xs) ++ " tests " ++ show (length (filter (==True) xs))
           ++ " of them passed " ++ show (length (filter (==False) xs)) ++   " of them failed"
