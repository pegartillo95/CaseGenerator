{-# LANGUAGE TemplateHaskell #-}

module TemplateAllv (
     gen_allv_str_listQ,
     gen_allv_list
    )where

import Language.Haskell.TH
import Data.Char
import Sized
import UUT

------------------------------------------------------------------------------------
-------------------- Usefull alias for some data types------------------------------
------------------------------------------------------------------------------------

-- A list of variables that bind in the constructor
type Cons_vars = [ExpQ]
--The type of the function that generates the body
type Gen_func = [Int] -> [Name]  -> [Name] -> [ExpQ]
-- The name of the instance function we will be creating
type Func_name = Name   
--Tuple that pairs the func_name and the function to generate the body
type Func = (Func_name, Gen_func)

gen_allv_str_listQ :: Q [String] -> Q [Dec]
gen_allv_str_listQ xs = do list <- xs
                           gen_allv_str_list list

gen_allv_str_list :: [String] -> Q [Dec]
gen_allv_str_list [] = return []
gen_allv_str_list (x:xs) = do
                              dec <- gen_allv_str x
                              rec <- gen_allv_str_list xs
                              return (dec:rec)

gen_allv_str :: String -> Q Dec
gen_allv_str str = do
                     (typeStr, numArg) <- return (headArgs(split_str str ""))
                     (Just typeName) <- lookupTypeName typeStr 
                     gen_allv typeName numArg
                        where
                          headArgs :: [String] -> (String,Int)
                          headArgs (x:xs) = (x, (length xs))
                          split_str :: String -> String -> [String]
                          split_str [] saved = [saved]
                          split_str (x:xs) saved
                            | x == ' ' = saved:(split_str xs "")
                            |otherwise = split_str xs (saved++[x])


gen_allv_list :: Name -> Int -> Q [Dec]
gen_allv_list name n = do 
                         dec <- gen_allv name n
                         return [dec]

-- Generate an intance of the class Allv for the type typName
gen_allv :: Name -> Int -> Q Dec
gen_allv t n =
  do (TyConI d) <- reify t
      --Extract all the type info of the data type 
     (t_name,noSimplifiedName,cInfo,consts,typesCons, isRec) <- typeInfo (return d)
     --We call to gen_instance with a name for the class, the name of the constructor,
     --a list of info of the constructors, the constructors itself, lists containing
     --the constructors, name of the data-type without being simplified, and lastly
     --the function to generate the body of the function of the class.            
     i_dec <- gen_instance (mkName "Allv") t_name cInfo consts
                            typesCons noSimplifiedName (mkName "allv", gen_body) n isRec
     return i_dec -- return the instance declaration
            -- gen_body is the function that we pass as an argument to gen_instance
            --and later on is used to generate the body of the allv function 
            --for a determined data-type
       where gen_body :: [Int] -> [Name] -> [Name]-> [ExpQ]
             gen_body _ [] [] = []
             gen_body (i:is) (c:cs) (f:fs) --cInfo consts listOfF 
                | null cs = [listExps] ++
                              (gen_body is cs fs)
                | otherwise = [appsE (varE '(++):[listExps] ++ gen_body is cs fs)]
                      where --constructorF decided to use the data constructor
                            --if having just one parameter or to use a function if
                            --having more than one. This is duo to the fact that
                            --if the data constructor has more than one parameter
                            --we need to apply compose to them and then apply a
                            --function over the result of compose.
                            listExps = if(i > 0) then appsE (mapE:constructorF:(allvFunc i))
                                       else listE [appsE (constructorF:[])]

                            constructorF 
                                | i > 1 = varE f
                                | otherwise = conE c
                            --mapE, composeE and allvE are three auxiliar function
                            --that serve to get the expresion equivalent to those 3
                            --functions in template haskell
                            mapE = varE 'map
                            composeE = varE 'compose
                            allvE = appsE [varE 'allv]
                            moveHead (x1:x2:xs) = x2:x1:xs

                            allvFunc 0 = []
                            allvFunc 1 = [allvE]
                            allvFunc n = [appsE (composeE:[allvE] ++ allvFunc (n-1))]


--Construct an instance of class class_name for type for_type
--with a corresponding function  to build the method body
gen_instance :: Name -> Name -> [Int] -> [Name]
                -> [[Type]] -> Name -> Func -> Int -> [Bool] -> DecQ
gen_instance class_name for_name cInfo consts typesCons typeName_nosimp func n isRec =
  instanceD (cxt (map applyConst ctxTypes))
    (appT (conT class_name) (foldl appT (conT for_name) (map varT ctxTypes))) 
    [(func_def func)] 
      where func_def (func_name, gen_func)-- extracts func_name and gen_func
                = funD func_name -- method name
                  -- generate function body
                  [gen_clause gen_func cInfo consts typesCons typeName_nosimp isRec]
            applyConst var_name = appT (conT class_name) (varT var_name)

            ctxTypes :: [Name]
            ctxTypes = map (\x -> mkName ("x"++ (show x))) (take n [1..])


-- Generate the pattern match and function body for a given method and
-- a given data-type. gen_func is the function that generates the function body
gen_clause :: Gen_func -> [Int] -> [Name] -> [[Type]] -> Name -> [Bool] -> ClauseQ
gen_clause gen_func cInfo consts typesCons typeName_nosimp isRec = 
      (clause []
             --here we execute the gen_function to generate the body of the function
            (normalB $ head (gen_func cInfoOrd constsOrd listOfFOutOrd))
             --this other one generates the where clause of the function
             (gen_wheres cInfoOrd constsOrd listOfFOutOrd))
      where --listOfFOut generates a fresh list of "Name" for n different f's
            --this f's are used when one of the data types has more than one
            --parameter 
            listOfFOut = listOfF (length consts)
            listOfF 0 = []
            listOfF n = (mkName ("f"++ show n)):(listOfF (n-1))
            --isRec checks which of the constructors of the given data-type
            --are recursive and which others are not. It returns a boolean list
            --where true means to be recursive and false to not to be recursive.
            --isRec = isRecAux typesCons
            --isRecAux [] = []
            --isRecAux (x:xs) = (or $ map (==(ConT typeName_nosimp)) x): isRecAux xs
            --ReorderL reorders all this lists so they have all non recursive
            --type constructors first and all recursive ones at the end
            reorderL = auxFirst cInfo consts listOfFOut isRec 0 False
            auxFirst is cs fs rs n foundRec
                |n > ((length rs)-1) = (is, cs, fs, rs)
                |foundRec && (not (rs!!n)) = auxFirst 
                                              ((is!!n):(remove n is 0)) 
                                              ((cs!!n):(remove n cs 0)) 
                                              ((fs!!n):(remove n fs 0)) 
                                              ((rs!!n):(remove n rs 0)) 0 False
                |(not foundRec) && (rs!!n) = auxFirst is cs fs rs (n+1) (not foundRec)
                |otherwise = auxFirst is cs fs rs (n+1) foundRec
            --removes position n from the list (x:xs)
            remove n (x:xs) actPos
                |n==actPos = xs
                |otherwise = x:(remove n xs (actPos+1))

            --This four functions serve to take the reordered lists for those 4 lists
            cInfoOrd = (\(x,_,_,_) -> x) reorderL
            constsOrd = (\(_,x,_,_) -> x) reorderL
            listOfFOutOrd = (\(_,_,x,_) -> x) reorderL
            isRecOrd = (\(_,_,_,x) -> x) reorderL


            --gen_wheres is the auxiliar function that generates the where "clause"
            --of the function when necesary.
            gen_wheres [] [] [] = []
            gen_wheres (n:ns) (c:cs) (f:fs) --gen_wheres numParam consts listOfF 
                | n > 1 = funD f (bodyFunc listOfVar c):gen_wheres ns cs fs
                | otherwise = gen_wheres ns cs fs
                  where listOfVar = listVariab n
                        listVariab 0 = []
                        listVariab n = (mkName ("x"++ show n)):(listVariab (n-1))
            --generates the body for the functions in the where clause when necessary.
            bodyFunc listOfVar constructorName = [clause (tupleParam listOfVar)
                                                  (normalB (appsE ((conE constructorName):
                                                    (map varE listOfVar)))) []]

            tupleParam (v:vs) --tupleParam listVars
                | (null vs) = [varP v]
                | otherwise = [tupP ((varP v):tupleParam vs)]


--Extracting information of the of the declaration of the data type, these are:
--  > The name of the type simplified (without the module name)
--  > The name of the type without being simplified
--  > A list of integers one for each constructor expresing the number of
--      arguments of each of them.
--  > A list of names of the different type constructors.
--  > List of lists each of the inner list having the types of the parameters of
--      data constructor.
typeInfo :: DecQ -> Q (Name, Name,[Int],[Name],[[Type]], [Bool])
typeInfo m =
     do d <- m
        case d of
           d@(DataD _ _ _ _ _) ->
            return $ (simpleName $ name d, name d , consA d, termsA d, listTypesA d, isRec (name d) (listTypesA d))
           d@(NewtypeD _ _ _ _ _) ->
            return $ (simpleName $ name d, name d , consA d, termsA d, listTypesA d, isRec (name d) (listTypesA d))
           _ -> error ("derive: not a data type declaration: " ++ show d)
 
     where
        consA (DataD _ _ _ cs _)    = map conA cs
        consA (NewtypeD _ _ _ c _)  = [ conA c ]

        conA (NormalC c xs)         = length xs
        conA (RecC c xs)            = length xs
        conA (InfixC _ c _)         = 2
 
        nameFromTyVar (PlainTV a)    = a
        nameFromTyVar (KindedTV a _) = a
 
        termsA (DataD _ _ _ cs _)   = map termA cs
        termsA (NewtypeD _ _ _ c _) = [ termA c ]
 
        termA (NormalC c xs)      = c
        termA (RecC c xs)         = c
        termA (InfixC t1 c t2)    = c
 
        name (DataD _ n _ _ _)      = n
        name (NewtypeD _ n _ _ _)   = n
        name d                      = error $ show d

        listTypesA (DataD _ _ _ cs _)    = (map typesA cs)
        listTypesA (NewtypeD _ _ _ c _)  = [ typesA c ]

        typesA (NormalC _ xs)         = map snd xs
        typesA (RecC _ xs)            = map (\(_, _, t) -> t) xs
        typesA (InfixC t1 _ t2)       = [snd t1] ++  [snd t2]

 
simpleName :: Name -> Name
simpleName nm =
   let s = nameBase nm
   in case dropWhile (/=':') s of
        []          -> mkName s
        _:[]        -> mkName s
        _:t         -> mkName t

isRec :: Name -> [[Type]] -> [Bool]
isRec name xs = map (\x -> isRecAux name x) xs

isRecAux :: Name -> [Type] -> Bool
isRecAux name xs = or (map (\x -> isRecType name x) xs)

isRecType :: Name -> Type -> Bool
isRecType rN t@(VarT _) = False
isRecType rN t@(ConT n) = n == rN
isRecType rN t@(ListT) = False
isRecType rN t@(TupleT 2) = False
isRecType rN t@(TupleT 3) = False
isRecType rN t@(TupleT 4) = False  
isRecType rN t@(AppT x y) = (isRecType rN x) || (isRecType rN y)
