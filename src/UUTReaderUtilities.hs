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

----------Recognize user defined types --------------------------------
notDefTypesQMonad :: Q [String] -> Q [String]
notDefTypesQMonad xs = do list <- xs
                          return (notDefTypes list)

notDefTypes :: [String] -> [String]
notDefTypes xs = removeDuplicates notDefinedTypes
                  where notDefinedTypes = concat (map defTypes separated)
                        separated = map (\x -> split_str x "") xs
                        
                        split_str :: String -> String -> [String]
                        split_str [] saved = [saved]
                        split_str (x:xs) saved
                          | x == ' ' = saved:(split_str xs "")
                          |otherwise = split_str xs (saved++[x])

                        defTypes :: [String] -> [String]
                        defTypes [] = []
                        defTypes (x:[])
                          | (definedBaseTypes x) = []
                          | otherwise = [x]
                        defTypes (x:xs) = (keepUndefinedComplex x (length xs))++(keepUndefinedSimples xs)

                        keepUndefinedComplex :: String -> Int -> [String]
                        keepUndefinedComplex x n
                          | (definedComplexTypes x) = []
                          | otherwise = [x ++ " " ++(listOfVariables n)]

                        listOfVariables :: Int -> String
                        listOfVariables 0 = ""
                        listOfVariables 1 = "x1"
                        listOfVariables n = ("x"++(show n)) ++ " " ++ (listOfVariables (n-1))
                        
                        keepUndefinedSimples :: [String] -> [String]
                        keepUndefinedSimples [] = []
                        keepUndefinedSimples (x:xs)
                          | (definedBaseTypes x) = keepUndefinedSimples xs
                          | otherwise = x:(keepUndefinedSimples xs)

definedBaseTypes :: String -> Bool  
definedBaseTypes x = (x == "Int" || x == "Char" || x == "Bool")

definedComplexTypes :: String -> Bool  
definedComplexTypes x = (x == "[]" || x == "(,)" || x == "(,,)" || x == "(,,,)" || x == "Array")

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

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
--test = prueba listArgs
--        where listArgs = (smallest :: $(inputT (head uutMethods)))