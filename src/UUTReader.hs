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

test_UUT :: Q Bool
test_UUT = do
              callGenerators
              callGenTest
              callGenAuxPrueba
              mainDriver

-------------call to gen_all and gen_arbitrary ----------------------------
callGenerators :: Q Bool 
callGenerators = do 
                    $(gen_allv_str_list (notDefTypes uutTypesStr))
                    $(gen_arb_str_list (notDefTypes uutTypesStr))
                    return True


--------------calls the function that generates test function------------
callGenTest = $(genTest)

--------------------Generar funciones auxiliares de prueba-------------
callGenAuxPrueba = do
                      $(gen_prec_lambda (head uutMethods))
                      $(gen_fun_f (head $ tail uutMethods))
                      $(gen_pos_lambda (head $ tail $ tail uutMethods) uutNargs)

----------------Generate the main driver loop --------------------------------------
mainDriver = $(gen_driver_loop)

gen_driver_loop = appsE ((varE 'test):[varE 'uutMethods])