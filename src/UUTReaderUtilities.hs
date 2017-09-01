{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module UUTReaderUtilities where

import Control.Monad
import Language.Haskell.TH
--import TemplateAllv
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
prueba listArgs = pos_f filtered_pre output
              where 
                     filtered_pre = pre_f listArgs
                     output = fun_f filtered_pre

pre_f listArgs = filter (prec_f_aux) listArgs

fun_f filtered_list = map fun_f_aux filtered_list

pos_f inputs outputs = zipWith pos_f_aux inputs outputs

--pos_f [] [] = []
--pos_f (l:ls) (o:os) = (pos_f_aux l o):(pos_f ls os)

--------------generators for auxiliar prueba functions-------------------------
prec_f_aux $(tupleP uutNargs) = $(appsE ((varE 'uutPrec):(map varE (listVar uutNargs))))

fun_f_aux $(tupleP uutNargs) = $(appsE ((varE 'uutMethod):(map varE (listVar uutNargs))))

pos_f_aux $(tupleP uutNargs) $(varP $ mkName "o") = $(appsE ((varE 'uutPost):((map varE (listVar uutNargs))++[varE $ mkName "o"]))) 

----------------------test function-------------------------------------
test = prueba listArgs
        where listArgs = smallest

----------------Get types for the input params------------------------------
get_f_inp_types :: String -> Q [String]
get_f_inp_types str = do (Just name) <- lookupValueName str
                         (_,_,text) <- extract_info (reify name)
                         t <- return(simplifyParsing text)
                         return (extract_types t [] "")
 
 
 --------Auxiliar functions for getFInpTypes ------------------------------
 
extract_info :: InfoQ -> Q(Name, Name, String)
extract_info m =
      do d <- m
         case d of
            d@(VarI _ _ _ _) ->
             return $ (funcName d, simpleName $ funcName d, parseDataTypes d)
            d@(ClassOpI _ _ _ _) ->
             return $ (funcName d, simpleName $ funcName d, parseDataTypes d)
            _ -> error ("Error in extract_info" ++ show d)
      where
         funcName (VarI n _ _ _)  = n
         funcName (ClassOpI n _ _ _) = n
 
         parseDataTypes (VarI _ x _ _) = first_parse x
         parseDataTypes (ClassOpI _ x _ _) = first_parse x
 
         first_parse ((ForallT _ _ x)) = parsing x
         first_parse x = parsing x
  
         parsing ((AppT (ConT x) (VarT y))) = "{" ++ (parsing (ConT x)) ++ "_" ++ (parsing (VarT y)) ++ "}"  
         parsing ((AppT x y)) = (parsing x) ++ (parsing y)
         parsing (ArrowT) = "-> "
         parsing (ListT) = "[] "
         parsing ((TupleT 2)) = "(,) "
         parsing ((TupleT 3)) = "(,,) "
         parsing ((VarT _)) = "Int "
         parsing ((ConT x)) = (nameBase x) ++ " " --possibility (eliminateMaybe (nameModule x)) ++ "." ++
         parsing _ = "UND "
 
 
simpleName :: Name -> Name
simpleName nm =
    let s = nameBase nm
    in case dropWhile (/=':') s of
         []          -> mkName s
         _:[]        -> mkName s
         _:t         -> mkName t
 
eliminateMaybe :: Maybe a -> a
eliminateMaybe (Just a) = a
 
simplifyParsing :: String -> String
simplifyParsing string = fst (auxiliarParse string)
 
auxiliarParse s
     | startswith "->" (lstrip s) = ( (fst call2) ++ " -> " ++ (fst (call 0 (snd call2))) , snd (call 0 (snd call2)))
     | startswith "[]" (lstrip s) = ("[" ++ (fst call2) ++ "]", snd call2)
     | startswith "(,)" (lstrip s) = ("(" ++ (fst call3) ++ "," ++ (fst (call 0 (snd call3))) ++ ")" , snd (call 0 (snd call3)))
     | startswith "(,,)" (lstrip s) = ("(" ++ (fst call4) ++ "," ++ (fst (call 0 (snd call4))) ++ "," ++ (fst (call 0 (snd (call 0 (snd call4))))) ++ ")" , snd (call 0 (snd (call 0 (snd call4)))))
     | startswith "(,,,)" (lstrip s) = ("(" ++ (fst call5) ++ "," ++ (fst (call 0 (snd call5))) ++ "," ++ (fst (call 0 (snd (call 0 (snd call5))))) ++ "," ++ (fst (call 0 (snd (call 0 (snd (call 0 (snd call5))))))) ++ ")" , snd (call 0 (snd (call 0 (snd (call 0 (snd call5)))))))
     | startswith "{" (lstrip s) = compoundVar (lstrip s) ""
     | otherwise = baseVar (lstrip s) ""
 
     where call2 = call 2 s
           call3 = call 3 s
           call4 = call 4 s
           call5 = call 5 s
           call n string = auxiliarParse (stringSkip n (lstrip string))
           stringSkip n (x:xs)
              | n == 0 = (x:xs)
              | otherwise = stringSkip (n-1) xs
           baseVar (x:xs) formedS
              | x /= ' '  = baseVar xs (formedS ++ [x])
              | otherwise = (formedS , (lstrip xs))
           compoundVar (x:xs) formedS
              | x == '}' = (formedS , (lstrip xs))
              | x == '{' = compoundVar xs formedS
              | otherwise = compoundVar xs (formedS ++ [x])

 -------------Extracts the input types from the simplify parsing----------
 
extract_types :: String -> [String] -> String -> [String]
extract_types [] list _ = list
extract_types (x1:xs) list building_t
      | (x1 == ' ') || (x1 == '>') = extract_types xs list building_t
      | (x1 == '-') = extract_types xs (list++[building_t]) ""
      | (x1 == '_') = extract_types xs list (building_t ++ " ")
      | otherwise = extract_types xs list (building_t++[x1])