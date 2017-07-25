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

test_UUT :: Q [Bool]
test_UUT = mainDriver

----------------Generate the main driver loop --------------------------------------
mainDriver = $(gen_driver_loop)

-------------call to gen_all and gen_arbitrary -------------------------------------
$(gen_allv_str_list (notDefTypes uutTypesStr))
$(gen_arb_str_list (notDefTypes uutTypesStr))

--------------Printing the ending information---------------------------------------

printInfo :: [Bool] -> String
printInfo xs = "Tried " ++ show (length xs) ++ " tests " ++ show (length (filter (==True) xs))
           ++ " of them passed " ++ show (length (filter (==False) xs)) ++ " of them failed"
