{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module UUTReaderUtilities where

import Control.Monad
import Language.Haskell.TH
import TemplateAllv
import Sized
import Arbitrary
import Data.String.Utils
import System.IO.Unsafe
import UUT
import UUTReaderUtilitiesDeep

----------Recognize user defined types --------------------------------
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

--------------generators for auxiliar prueba functions-------------------------
prec_lambda = $(lamE [(tupP (map varP (listVar uutNargs)))] (appsE ((varE 'uutPrec):(map varE (listVar uutNargs)))))

fun_f filtered_list = map fun_f_aux filtered_list

fun_f_aux = $(lamE [tupP (map varP (listVar uutNargs))] (appsE ((varE 'uutMethod):(map varE (listVar uutNargs)))))

pos_lambda = $(lamE [(tupP (map varP (listVar uutNargs))), varP $ mkName "o"] (appsE ((varE 'uutPost):((map varE (listVar uutNargs))++[varE $ mkName "o"]))))

--------------------Prueba function------------------------------------
prueba listArgs = do
                    filtered_pre <- return (pre_f listArgs)
                    output <- return (fun_f filtered_pre)
                    return (pos_f filtered_pre output)

pre_f listArgs = filter (prec_lambda) listArgs

pos_f [] [] = []
pos_f (l:ls) (o:os) = (pos_lambda l o):(pos_f ls os)

--------------calls the function that generates test function------------
$(genTest)

----------------------generates driver loop-----------------------------
gen_driver_loop = appsE ((varE 'test):[varE 'uutMethods])