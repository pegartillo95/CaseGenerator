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
import UUTReaderUtilitiesDeep

test_UUT = test

-------------call to gen_all and gen_arbitrary -------------------------------------
$(gen_allv_str_listQ (notDefTypesQMonad (get_f_inp_types (head uutMethods))))

--------------Printing the ending information---------------------------------------

printInfoTuple (xs,ys,zs) = printInfo xs ys zs

printInfo xs ys zs = "Testing the function " ++ uutName ++ " generating a total of " ++ (show (length ys)) ++
                     " test cases of which " ++ (show (length zs)) ++ " passed the precondition "
                    ++ "\n\nTest cases:" ++ (printTestCases ys)
                    ++ "\n\nTest cases that passed the precondition:" ++ (printTestCases zs) ++
                    if ((length $ filter (== True) xs) == (length xs)) then " None of them failed"
                      else "And these are the ones that failed. \n" ++ (printInfoAux xs ys)

printInfoAux [] [] = ""
printInfoAux (x:xs) (y:ys)
    | (x == True) = (printInfoAux xs ys)
    | otherwise = (show y) ++ (printInfoAux xs ys)

printTestCases [] = ""
printTestCases (y:ys) = (show y) ++ ", " ++ (printTestCases ys)

----------------------test function-------------------------------------
test = prueba listArgs
        where listArgs = (smallest :: $(inputT (head uutMethods)))
