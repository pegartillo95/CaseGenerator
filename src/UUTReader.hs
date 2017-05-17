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

--reads from the UUT archive the number of methods to tests 
--them one by one.
{-read_UUT = test_UUT uutNMethods

test_UUT n
   | n > 0 = do
   	           reified <- reify_func ("uutPrec_" ++ (show n))
               --info <- extract_info reified


reify_func :: String -> Q Info
reify_func string = $(lookupValueName string >>= 
                        (\(Just name) -> reify name))-}

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
        parsing ((VarT _)) = "Int "
        parsing ((ConT x)) = (nameBase x) ++ " "
        parsing _ = "UND "


simpleName :: Name -> Name
simpleName nm =
   let s = nameBase nm
   in case dropWhile (/=':') s of
        []          -> mkName s
        _:[]        -> mkName s
        _:t         -> mkName t

simplifyParsing string = fst (auxiliarParse string)

auxiliarParse s
    | startswith "->" (lstrip s) = ( "("++(fst call2) ++ " -> " ++ (fst (call 0 (snd call2))) ++ ")" , snd (call 0 (snd call2)))
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
          