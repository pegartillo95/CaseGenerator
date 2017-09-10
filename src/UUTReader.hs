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
import TemplateSized
import UUT
import UUTReaderUtilities
import UUTReaderUtilitiesDeep

-------------call to gen_all and gen_arbitrary -------------------------------------
$(gen_allv_str_listQ (notDefTypesQMonad (get_f_inp_types (head uutMethods))))
-------------call to gen_sized and gen_arbitrary -------------------------------------
$(gen_sized_str_listQ (notDefTypesQMonad (get_f_inp_types (head uutMethods))))

------------------main test function------------------------------------------------
test_UUT = test

--------------Printing the ending information---------------------------------------

printInfoTuple (xs,ys,zs) = printInfo xs ys zs

printInfo xs ys zs = "Testing the function " ++ uutName ++ " generating a total of " ++ (show (length ys)) ++
                     " test cases of which " ++ (show (length zs)) ++ " passed the precondition "
                    ++ "\n\n\nTest cases:" ++ (printTestCases ys)
                    ++ "\n\n\nTest cases that passed the precondition:" ++ (printTestCases zs) ++
                    if ((length $ filter (== True) xs) == (length xs)) then " None of them failed"
                      else "\n\n\nAnd these are the ones that failed.\n" ++ (printInfoAux xs zs)

printInfoAux [] [] = ""
printInfoAux (x:xs) (y:ys)
    | (x == True) = (printInfoAux xs ys)
    | otherwise = (show y) ++ "\n" ++ (printInfoAux xs ys)

printTestCases [] = ""
printTestCases (y:ys) = (show y) ++ ", " ++ (printTestCases ys)

----------------------test function-------------------------------------
test = prueba listArgs
        where listArgs = (smallest :: $(inputT (head uutMethods)))
