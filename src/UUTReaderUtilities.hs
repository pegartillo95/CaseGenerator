{-# LANGUAGE TemplateHaskell #-}

module UUTReaderUtilities where

import Language.Haskell.TH
import TemplateAllv

----------------------constantes default------------------------------
numCases::Int
numCases = 1000

smallestSize::Int
smallestSize = 4

----------------------auxiliar functs for template hasskell-----------
listVar :: Int -> [Name]
listVar 0 = []
listVar n = (mkName ("x"++ show n)):(listVar (n-1))

tupleParam :: [Name] -> [PatQ]
tupleParam listOfVar = [tupP (map varP listOfVar)]

body :: String -> [Name] -> ExpQ
body f_name listOfVar = (lookupValueName f_name >>=
                          (\(Just name)->(appsE ((varE name):(map varE listOfVar)))
	                        ))
	                       

body2 :: String -> [Name]-> ExpQ
body2 f_name listOfVar = (lookupValueName f_name >>=
                          (\(Just name)->(appsE ((varE name):((map varE listOfVar)++[varE nameY])))
	                        ))

nameY :: Name
nameY = mkName "y"






---------------Get types for the input params------------------------------
get_f_inp_types :: String -> Q [String]
get_f_inp_types str = (lookupValueName str >>= 
                        (\(Just name) -> extract_info (reify name) >>=
                           (\(_,_,text) -> return(simplifyParsing text) >>=
                              (\t -> return(extract_types t [] "")
                                ))))


--------Auxiliar functions for get_f_inp_types ------------------------------

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
        parsing ((VarT _)) = "GHC.Types.Int "
        parsing ((ConT x)) = (eliminateMaybe (nameModule x)) ++ "." ++(nameBase x) ++ " " 
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


-------------Extracts the input types from the simplify parsing----------

extract_types :: String -> [String] -> String -> [String]
extract_types [] list _ = list
extract_types (x1:xs) list building_t
     | (x1 == ' ') || (x1 == '>') = extract_types xs list building_t
     | (x1 == '-')= extract_types xs (list++[building_t]) ""
     | otherwise = extract_types xs list (building_t++[x1])