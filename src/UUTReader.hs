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

{-test_UUT :: Q Bool
test_UUT = do
              callGenerators
              callGenTest uutTestMode
              callGenPrueba
              mainDriver-}

-------------call to gen_all and gen_arbitrary ----------------------------
callGenerators :: Q Bool 
callGenerators = do 
                    _ <- $(gen_allv_str_list (notDefTypes uutTypesStr))
                    _ <- $(gen_arb_str_list (notDefTypes uutTypesStr))
                    return True


-------------Generate test function -------------------------------------
callGenTest x
  | x == 0 = callGenTest_sized
  | x == 1 = callGenTest_smallest
  | otherwise = callGenTest_arb

callGenTest_sized = $(genTest 0)
callGenTest_smallest = $(genTest 1)
callGenTest_arb = $(genTest 2)

genTest :: Int -> Q [Dec] --The integer means sized(0), smallest(1), arbitrary(2)
genTest x = do
               prueba <- mkName ("prueba")
               uutMethods <- mkName "uutNameMethods"
               listArg <- mkName "listArg"
               test <- testN prueba uutMethods listArg
               wherecl <- funD listArg [clause [] (normalB(build_where x)) []]
               args <- (varP uutMethods)
               let name = mkName $ "test"
               return [FunD name [Clause [args] (NormalB test) [wherecl]]]

testN :: Name -> Name -> Name -> ExpQ
testN pName nameMeth listArg = do appsE $ map varE (pName:nameMeth:listArg)

build_where :: Int -> ExpQ
build_where x
  | x==0 = appsE ((zipN uutNargs):(take uutNargs (repeat(appsE [varE 'sized]))))
  | x==1 = appsE ((zipN uutNargs):(take uutNargs (repeat(appsE [varE 'smallest]))))
  | x==2 = appsE ((zipN uutNargs):(take uutNargs (repeat(appsE [varE 'arbitrary]))))

--------------------Generate pruebas function------------------------------------

genPruebas :: Q [Dec]
genPruebas = do
                  name <- mkName ("prueba")
                  f <- varP $ mkName "f"
                  pruebaEmpty <- appsE ((varE 'return):[listE []])
                  prueba <- pruebaN --TODO prueba for non empty list
                  emptyList <- listP []
                  args1 <- (f:[emptyList])
                  args2 <- --TODO args2 the one of the recursive call
                  return $ FunD name [Clause [args1] (NormalB pruebaEmpty) [], Clause [args2] (NormalB prueba) []]

----------------Generate the main driver loop --------------------------------------
mainDriver = $(gen_driver_loop)

gen_driver_loop = appsE ((varE 'test):[varE 'uutMethods])