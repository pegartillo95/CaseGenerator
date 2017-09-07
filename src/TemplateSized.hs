{-# LANGUAGE TemplateHaskell #-}

module TemplateSized (
     gen_sized_str_listQ
    )where

import Language.Haskell.TH
import Data.Char
import Sized


gen_sized_str_listQ :: Q [String] -> Q [Dec]
gen_sized_str_listQ xs = do list <- xs
                            gen_sized_str_list list

gen_sized_str_list :: [String] -> Q [Dec]
gen_sized_str_list [] = return []
gen_sized_str_list (x:xs) = do
                               dec <- gen_sized_str x
                               rec <- gen_sized_str_list xs
                               return (dec:rec)

gen_sized_str :: String -> Q Dec
gen_sized_str str = do
                     listStr <- return(split_str str "")
                     listNames <- strs_to_names listStr 
                     gen_sized listNames
                        where
                          split_str :: String -> String -> [String]
                          split_str [] saved = [saved]
                          split_str (x:xs) saved
                            | x == ' ' = saved:(split_str xs "")
                            |otherwise = split_str xs (saved++[x])
                          strs_to_names [] = return []   
                          strs_to_names (x:xs) = do 
                                                   (Just n) <- lookupValueName x
                                                   rec <- strs_to_names xs
                                                   return (n:rec)

-- Generate an intance of the class Allv for the type typName
gen_sized :: [Name] -> Q Dec
gen_sized (t:ts) =
  do            
     i_dec <- gen_instance (mkName "Allv") t ts
     return i_dec -- return the instance declaration

gen_instance :: Name -> Name -> [Name] -> DecQ
gen_instance class_name for_name ctxTypes =
  instanceD (cxt (map applyConst ctxTypes))
    (appT (conT class_name) (foldl appT (conT for_name) (map varT ctxTypes))) 
    []
    where applyConst var_name = appT (conT class_name) (varT var_name)