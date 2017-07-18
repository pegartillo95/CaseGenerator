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
test_UUT = {-do
              callGenerators
              callGenTest
              callGenAuxPrueba-}
              mainDriver

----------------Generate the main driver loop --------------------------------------
mainDriver = $(gen_driver_loop)

-------------call to gen_all and gen_arbitrary ----------------------------
$(gen_allv_str_list (notDefTypes uutTypesStr))
$(gen_arb_str_list (notDefTypes uutTypesStr))

{-callGenerators :: Q Bool 
callGenerators = do 
                    $(gen_allv_str_list (notDefTypes uutTypesStr))
                    $(gen_arb_str_list (notDefTypes uutTypesStr))
                    return True-}




{-callGenTest = $(genTest)-}

--------------------Generar funciones auxiliares de prueba-------------
{-callGenAuxPrueba = do
                      $(gen_prec_lambda (head uutMethods))
                      $(gen_fun_f (head $ tail uutMethods))
                      $(gen_pos_lambda (head $ tail $ tail uutMethods) uutNargs)-}