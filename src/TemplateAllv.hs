{-# LANGUAGE TemplateHaskell #-}

module TemplateAllv (
     gen_allv
    , compose
    )where

import Language.Haskell.TH
import Data.Char
import Sized

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

gen_allv_str_list :: [String] -> Q [Dec]
gen_allv_str_list [] = return []
gen_allv_str_list (x:xs) = do
                              dec <- gen_allv_str x
                              rec <- gen_allv_str_list xs
                              return (dec:rec)

gen_allv_str :: String -> Q Dec
gen_allv_str str = do
                     (Just name) <- lookupValueName str
                     gen_allv name

-- Generate an intance of the class Allv for the type typName
gen_allv :: Name -> Q Dec
gen_allv typName =
  do (TyConI d) <- reify typName
      --Extract all the type info of the data type 
     (t_name,noSimplifiedName,cInfo,consts,typesCons) <- typeInfo (return d)
     --We call to gen_instance with a name for the class, the name of the constructor,
     --a list of info of the constructors, the constructors itself, lists containing
     --the constructors, name of the data-type without being simplified, and lastly
     --the function to generate the body of the function of the class.            
     i_dec <- gen_instance (mkName "Allv") (conT t_name) cInfo consts
                            typesCons noSimplifiedName (mkName "allv", gen_body)
     return i_dec -- return the instance declaration
            -- gen_body is the function that we pass as an argument to gen_instance
            --and later on is used to generate the body of the allv function 
            --for a determined data-type
       where gen_body :: [Int] -> [Name] -> [Name]-> [ExpQ]
             gen_body _ [] [] = []
             gen_body (i:is) (c:cs) (f:fs) --cInfo consts listOfF 
                | null cs = [appsE (mapE:constructorF:(allvFunc i))] ++
                              (gen_body is cs fs)
                | otherwise = [appsE (varE '(++):[appsE (mapE:constructorF:
                            (allvFunc i))] ++ gen_body is cs fs)]
                      where --constructorF decided to use the data constructor
                            --if having just one parameter or to use a function if
                            --having more than one. This is duo to the fact that
                            --if the data constructor has more than one parameter
                            --we need to apply compose to them and then apply a
                            --function over the result of compose.
                            constructorF 
                                | i > 1 = varE f
                                | otherwise = conE c
                            --mapE, composeE and allvE are three auxiliar function
                            --that serve to get the expresion equivalent to those 3
                            --functions in template haskell
                            mapE = varE 'map
                            composeE = varE 'compose
                            allvE = varE 'allv
                            moveHead (x1:x2:xs) = x2:x1:xs

                            allvFunc 1 = [appsE [allvE]]
                            allvFunc n = [appsE (composeE:[allvE] ++ allvFunc (n-1))]


--Construct an instance of class class_name for type for_type
--with a corresponding function  to build the method body
gen_instance :: Name -> TypeQ -> [Int] -> [Name]
                  -> [[Type]] -> Name -> Func -> DecQ
gen_instance class_name for_type cInfo consts typesCons typeName_nosimp func =
  instanceD (cxt [])
    (appT (conT class_name) for_type) 
    [(func_def func)] 
      where func_def (func_name, gen_func)-- extracts func_name and gen_func
                = funD func_name -- method name
                  -- generate function body
                  [gen_clause gen_func cInfo consts typesCons typeName_nosimp]


-- Generate the pattern match and function body for a given method and
-- a given data-type. gen_func is the function that generates the function body
gen_clause :: Gen_func -> [Int] -> [Name] -> [[Type]] -> Name -> ClauseQ
gen_clause gen_func cInfo consts typesCons typeName_nosimp = 
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
            isRec = isRecAux typesCons
            isRecAux [] = []
            isRecAux (x:xs) = (or $ map (==(ConT typeName_nosimp)) x): isRecAux xs
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
typeInfo :: DecQ -> Q (Name, Name,[Int],[Name],[[Type]])
typeInfo m =
     do d <- m
        case d of
           d@(DataD _ _ _ _ _) ->
            return $ (simpleName $ name d, name d , consA d, termsA d, listTypesA d)
           d@(NewtypeD _ _ _ _ _) ->
            return $ (simpleName $ name d, name d , consA d, termsA d, listTypesA d)
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

------------------------------------------
--------Composing of 2 lists--------------
------------------------------------------

compose :: [a] -> [b] -> [(a,b)]
compose xs ys = (e:lattice)
  where e:lattice = concat $ diags 0 xs ys

--
-- It builds the lattice of tuples from two lists, each one may be either
-- finite or infinite


diags :: Int -> [a] -> [b] -> [[(a,b)]]
diags _ [] [] = [[]]
diags i xs ys
    | fullDiag     = [tup k | k <- [0..i]] : diags (i+1) xs ys
    | finiteFirst  = diags (i-1) xs  ysr
    | finiteSecond = diags (i-1) xsr ys
    | otherwise    = diags (i-2) xsr ysr

  where xs'          = drop i xs
        ys'          = drop i ys
        xsr          = tail xs
        ysr          = tail ys
        fullDiag     = not (null xs') && not (null ys')
        finiteFirst  = null xs' && not (null ys')
        finiteSecond = not (null xs') && null ys'
        tup k        = (x,y)
                       where x = xs !! k 
                             y = ys !! (i-k)


{-diags :: Int -> Int -> Int -> [a] -> [b] -> [[(a,b)]]
diags _ _ _ [] [] = [[]]
diags i dx dy xs ys
    | fullDiag     = [tup k | k <- [0..i]] : diags (i+1) dx dy xs ys
    | finiteFirst  = diags (i-1) dx     (dy+1) xs  ysr
    | finiteSecond = diags (i-1) (dx+1) dy     xsr ys
    | otherwise    = diags (i-2) (dx+1) (dy+1) xsr ysr

  where xs'          = drop i xs
        ys'          = drop i ys
        xsr          = tail xs
        ysr          = tail ys
        fullDiag     = not (null xs') && not (null ys')
        finiteFirst  = null xs' && not (null ys')
        finiteSecond = not (null xs') && null ys'
        tup k        = (x,y)
                       where x = xs !! k 
                             y = ys !! (i-k)-}