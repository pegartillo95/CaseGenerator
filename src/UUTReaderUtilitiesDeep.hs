{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module UUTReaderUtilitiesDeep where

import Control.Monad
import Language.Haskell.TH
import UUT
import Sized
import Data.String.Utils
import qualified Arrays as A
import qualified Bags as B
import qualified Sets as S
import qualified Sequences as Q
import Assertion
import Data.List

--------------Create a list of variables -----------------------------
listVar :: Int -> [Name]
listVar 0 = []
listVar n = (mkName ("x"++ show n)):(listVar (n-1))

-------------Linked tuples creator-------------------------------------

tupleP :: Int -> PatQ
tupleP n = tupP (listOfPs n)

listOfPs :: Int -> [PatQ]
listOfPs 0 = []
listOfPs n = (varP $ mkName ("x" ++ (show n))):(listOfPs (n-1))

----------------inputTypes for smallest ------------------------------------
inputT :: String -> TypeQ
inputT str = do
                (Just name) <- lookupValueName str
                listTypes <- getListTypes (reify name)
                monomTypes <- return (monomorphing listTypes)
                (ts, lastElem) <- return (separateLastElem monomTypes [])
                tupleType <- return(TupleT (length ts))
                genType <- return (foldl AppT tupleType ts)                         
                return (AppT ListT genType)

monomorphing :: [Type] -> [Type]
monomorphing [] = []
monomorphing ((VarT _):xs) = (ConT ''Int):(monomorphing xs)
monomorphing (x:xs) = x:(monomorphing xs)


separateLastElem :: [Type] -> [Type] -> ([Type], Type)
separateLastElem (x:[]) accum = (accum, x)
separateLastElem (x:xs) accum = separateLastElem xs (accum++[x])

getListTypes :: InfoQ -> Q [Type]
getListTypes info = 
        do d <- info
           return (typesList d)
        where 
           typesList (VarI _ x _ _) = firstParsing x
           typesList (ClassOpI _ x _ _) = firstParsing x

           firstParsing ((ForallT _ _ x)) = extracTypes x
           firstParsing x = extracTypes x
           
           extracTypes ((AppT (AppT ArrowT x) y)) = [x]++(extracTypes y)
           extracTypes x = [x]

           {-extracTypes ((AppT (AppT (TupleT 2) x) y)) = (AppT (AppT (TupleT 2) (extracTypes x)) (extracTypes y))
           extracTypes ((AppT (AppT (AppT (TupleT 3) x) y) z)) = (AppT (AppT (AppT (TupleT 3) (extracTypes x)) (extracTypes y)) (extracTypes z))
           extracTypes ((AppT ArrowT x)) = extracTypes x
           extracTypes ((AppT ListT x)) = (AppT ListT (extracTypes x))
           extracTypes ((AppT x y)) = (extracTypes x):[(extracTypes y)]
           extracTypes ((ConT x)) = (ConT x)
           extracTypes ((VarT _)) = (ConT ''Int)-}

{-inputTypesExt :: Q [String] -> TypeQ
inputTypesExt strsQ = do
                         strList <- strsQ
                         typeList <- return (inputTypes strList)
                         tupleType <- return(tupleT (length typeList))
                         genType <- return (foldl appT tupleType typeList)                         
                         (appT listT genType)

inputTypes :: [String] -> [TypeQ]
inputTypes [] = []
inputTypes (x:xs) 
   | (isSimple x) = (simpleType x):(inputTypes xs)
   | otherwise = (complexType x): (inputTypes xs)

isSimple :: String -> Bool
isSimple [] = True
isSimple (x:xs) = (x /=  ' ') && (isSimple xs) 

simpleType :: String -> TypeQ
simpleType str = do
                 --(Just name) <- lookupTypeName str
                 let name = ''Int
                 return (ConT name)

complexType :: String -> TypeQ
complexType str = do
                     listStr <- return(split_str str [])
                     listNames <- strs_to_names listStr
                     (t:ts) <- return (transformToType listNames)
                     genType <- return (foldl AppT t ts)
                     return genType
	                   where
                             split_str [] saved = [saved]
                             split_str (x:xs) saved
                               | x == ' ' = saved:(split_str xs [])
                               |otherwise = split_str xs (saved++[x])
                              strs_to_names [] = return []   
                             strs_to_names (x:xs) = do 
                                                      (Just n) <- lookupValueName x
                                                      rec <- strs_to_names xs
                                                      return (n:rec)
                             transformToType (x:xs) = (ConT x):(map VarT xs)-}

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