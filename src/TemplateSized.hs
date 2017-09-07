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
                     (typeStr, numArg) <- return (headArgs(split_str str ""))
                     (Just typeName) <- lookupTypeName typeStr 
                     gen_sized typeName numArg
                        where
                          headArgs :: [String] -> (String,Int)
                          headArgs (x:xs) = (x, (length xs))
                          split_str :: String -> String -> [String]
                          split_str [] saved = [saved]
                          split_str (x:xs) saved
                            | x == ' ' = saved:(split_str xs "")
                            |otherwise = split_str xs (saved++[x])

-- Generate an intance of the class Allv for the type typName
gen_sized :: Name -> Int -> Q Dec
gen_sized t n =
  do            
     i_dec <- gen_instance (mkName "Allv") t n
     return i_dec -- return the instance declaration

gen_instance :: Name -> Name -> Int -> DecQ
gen_instance class_name for_name n =
  instanceD (cxt (map applyConst ctxTypes))
    (appT (conT class_name) (foldl appT (conT for_name) (map varT ctxTypes))) 
    []
    where applyConst var_name = appT (conT class_name) (varT var_name)

          ctxTypes :: [Name]
          ctxTypes = map (\x -> mkName ("x"++ (show x))) (take n [1..])