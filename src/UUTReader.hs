{-# LANGUAGE TemplateHaskell #-}

module UUTReader where

import Data.String.Utils
import Language.Haskell.TH
import GHC.Generics
import Sized
import Arbitrary
import TemplateAllv
import TemplateArbitrary
import UUT
import UUTReaderUtilities

--reads from the UUT archive the number of methods to tests 
--them one by one.
{-read_UUT = test_UUT 1 uutMethods

test_UUT :: Int -> Int -> [Bool]
test_UUT i n
   | i < n = do Just name <- lookupValueName ("uutPrec-"++(show i))
                info <- extract_info (reify name)
                parsed_type <- (\(_,_,text) -> simplifyParsing text) info
                test_UUT (i+1) n
   | otherwise = do Just name <- lookupValueName ("uutPrec-"++(show i))
                    info <- extract_info (reify name)
                    return-}
     
get_f_inp_types :: String -> [String]
get_f_inp_types str = (lookupValueName str >>= 
                        (\(Just name) -> extract_info (reify name) >>=
                           (\(_,_,text) -> return(simplifyParsing text) >>=
                              (\t -> return(extract_types t [] "")
                                ))))



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
 
        parsing ((AppT x y)) = (parsing x) ++ (parsing y)
        parsing (ArrowT) = "-> "
        parsing (ListT) = "[] "
        parsing ((TupleT 2)) = "(,) "
        parsing ((TupleT 3)) = "(,,) "
        parsing ((VarT _)) = "Int " --GHC.Types.Int
        parsing ((ConT x)) = (nameBase x) ++ " " --Si es necesario (nameModule x) ++ "." ++ 
        parsing _ = "UND "


simpleName :: Name -> Name
simpleName nm =
   let s = nameBase nm
   in case dropWhile (/=':') s of
        []          -> mkName s
        _:[]        -> mkName s
        _:t         -> mkName t

simplifyParsing :: String -> String
simplifyParsing string = fst (auxiliarParse string)

auxiliarParse s
    | startswith "->" (lstrip s) = ( (fst call2) ++ " -> " ++ (fst (call 0 (snd call2))) , snd (call 0 (snd call2)))
    | startswith "[]" (lstrip s) = ("[" ++ (fst call2) ++ "]", snd call2)
    | startswith "(,)" (lstrip s) = ("(" ++ (fst call3) ++ "," ++ (fst (call 0 (snd call3))) ++ ")" , snd (call 0 (snd call3)))
    | startswith "(,,)" (lstrip s) = ("(" ++ (fst call4) ++ "," ++ (fst (call 0 (snd call4))) ++ "," ++ (fst (call 0 (snd (call 0 (snd call4))))) ++ ")" , snd (call 0 (snd (call 0 (snd call4)))))
    | startswith "(,,,)" (lstrip s) = ("(" ++ (fst call5) ++ "," ++ (fst (call 0 (snd call5))) ++ "," ++ (fst (call 0 (snd (call 0 (snd call5))))) ++ "," ++ (fst (call 0 (snd (call 0 (snd (call 0 (snd call5))))))) ++ ")" , snd (call 0 (snd (call 0 (snd (call 0 (snd call5)))))) )
    | otherwise = baseVar (lstrip s) ""



    where call2 = call 2 s
          call3 = call 3 s
          call4 = call 4 s
          call5 = call 5 s
          call n string = auxiliarParse (stringSkip n (lstrip string))
          stringSkip n (x:xs)
             | n == 0 = (x:xs)
             | otherwise = stringSkip (n-1) xs
          baseVar s formedS
             | (head s) /= ' '  = baseVar (tail s) (formedS ++ [head s])
             | otherwise = (formedS , (lstrip (tail s)))


-------------Extracts the input types from the simplify parsing-----------

extract_types :: String -> [String] -> String -> [String]
extract_types [] list _ = list
extract_types (x1:xs) list building_t
     | (x1 == ' ') || (x1 == '>') = extract_types xs list building_t
     | (x1 == '-')= extract_types xs (list++[building_t]) ""
     | otherwise = extract_types xs list (building_t++[x1])

----------------Functions to filter the list of cases by precondition---

{-filterPrec :: Name -> Int -> [a] -> [a] -> [a]
filterPrec _ _ [] solList = solList 
filterPrec f_name n (t:ts) solList = filterPrec f_name n ts (solList++x)
     where x = if (testP f_name n t) then [t]
               else []

        
---------Appplying the function to get the corresponding outputs---------
passFun :: Name -> Int -> [a] -> [a] -> [a]
passFun _ _ [] solList = solList 
passFun f_name n (t:ts) solList = passFun f_name n ts (solList++y)
     where y = testP f_name n t

testP :: Name -> Int -> a -> b
testP f_name n t = $(lamE (tupleParam (listVar n)) (body f_name (listVar n))) t


-----Function to test the postcondition----------------------------------
testPost :: Name -> Int -> [a] -> [b] -> Bool
testPost _ _ [] [] = True
testPost f_name n (t:ts) (o:os) = aux_bool && (testPost f_name n ts os)
     where aux_bool = post f_name n t o

post :: Name -> a -> b -> Bool
post f_name n t o = $(lamE ((tupleParam (listVar n))++[varP nameY]) (body2 f_name (listVar n)))-}