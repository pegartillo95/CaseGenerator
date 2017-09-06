{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module UUTReaderUtilities where

import Control.Monad
import Language.Haskell.TH
import Sized
import Arbitrary
import Data.String.Utils
import System.IO.Unsafe
import UUT
import UUTReaderUtilitiesDeep
import qualified Arrays as A
import qualified Bags as B
import qualified Sets as S
import qualified Sequences as Q
import Assertion
import Data.List

----------Recognize user defined types --------------------------------
notDefTypesQMonad :: Q [String] -> Q [String]
notDefTypesQMonad xs = do list <- xs
                          return (notDefTypes list)

notDefTypes :: [String] -> [String]
notDefTypes [] = []
notDefTypes (x:xs)
   | (isUserDef (plain x)) = (plain x):(notDefTypes xs)
   | otherwise = notDefTypes xs

isUserDef :: String -> Bool
isUserDef str
   | str == "Int" || str == "Char" || str == "Bool" = False
   | otherwise = True

plain :: String -> String
plain [] = []
plain (x:xs)
  | x == '[' || x == ']' = plain xs
  | otherwise = x:(plain xs)

--------------------Prueba function------------------------------------
prueba listArgs = ((pos_f filtered_pre output), (listArgs), (filtered_pre))
              where 
                     filtered_pre = pre_f listArgs
                     output = fun_f filtered_pre

pre_f listArgs = filter (prec_f_aux) listArgs

fun_f filtered_list = map fun_f_aux filtered_list

pos_f inputs outputs = zipWith pos_f_aux inputs outputs

--------------generators for auxiliar prueba functions-------------------------
prec_f_aux $(tupleP uutNargs) = $(appsE ((varE 'uutPrec):(map varE (listVar uutNargs))))

fun_f_aux $(tupleP uutNargs) = $(appsE ((varE 'uutMethod):(map varE (listVar uutNargs))))

pos_f_aux $(tupleP uutNargs) $(varP $ mkName "o") = $(appsE ((varE 'uutPost):((map varE (listVar uutNargs))++[varE $ mkName "o"]))) 

----------------------test function-------------------------------------
test = prueba listArgs
        where listArgs = (smallest :: $(inputTypesExt (get_f_inp_types (head uutMethods))))