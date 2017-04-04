{-# LANGUAGE TemplateHaskell #-}

module TemplateAllv (
    MyExp(..)
    , gen_allv
    , compose
    )where

import Language.Haskell.TH
import Data.Char



------------------------------------------------------------------------------------
-------------------- Usefull alias for some data types------------------------------
------------------------------------------------------------------------------------

-- the list of consts
type Constructor = (Name, [(Maybe Name, Type)])
-- A list of variables that bind in the constructor
type Cons_vars = [ExpQ]
--The type of the function that generates the body
type Gen_func = [(Name, Int)] -> [Constructor]  -> [Name] -> [Bool] -> [ExpQ]
-- The name of the instance function we will be creating
type Func_name = Name   
--Tuple that pairs the func_name and the function to generate the body
type Func = (Func_name, Gen_func)

-- Generate an intance of the class TH_Render for the type typName
gen_allv :: Name -> Q Dec
gen_allv typName =
  do (TyConI d) <- reify typName
      --Extract all the type info of the data type 
     (t_name,cInfo,consts,typesCons, noSimplifiedName) <- typeInfo (return d)                
     i_dec <- gen_instance (mkName "Allv") (conT t_name) cInfo consts
                            typesCons noSimplifiedName (mkName "allv", gen_allv)
     return i_dec -- return the instance declaration
             -- function to generation the function body for a particular function
             -- and constructor
       where gen_allv _ [] [] [] = []
             gen_allv (i:is) (c:cs) (f:fs) (r:rs) --cInfo consts listOfF isRecList 
                | null cs = [appsE (mapE:constructorF:(allvFunc (snd i)))] ++
                              (gen_allv is cs fs rs)
                | not r = [appsE (varE '(++):[appsE (mapE:constructorF:
                            (allvFunc (snd i)))] ++ gen_allv is cs fs rs)]
                | not $ head rs = gen_allv (moveHead (i:is)) (moveHead (c:cs))
                                    (moveHead (f:fs)) (moveHead (r:rs))
                | otherwise = [appsE (varE '(++):[appsE (mapE:constructorF:
                                (allvFunc (snd i)))] ++ gen_allv is cs fs rs)]
                      where functE string = varE $ mkName string
                            constructorF 
                                | (snd i) > 1 = varE f
                                | otherwise = functE $ nameBase $ fst c
                            mapE = functE "map"
                            composeE = functE "compose"
                            allvE = functE "allv"
                            moveHead (x1:x2:xs) = x2:x1:xs

                            allvFunc 1 = [appsE [allvE]]
                            allvFunc n = [appsE (composeE:[allvE] ++ allvFunc (n-1))]

--TODO a partir de aqui.

-- construct an instance of class class_name for type for_type
-- funcs is a list of instance method names with a corresponding
-- function to build the method body
gen_instance :: Name -> TypeQ -> [(Name, Int)] -> [Constructor]
                  -> [[Type]] -> Name -> Func -> DecQ
gen_instance class_name for_type cInfo consts typesCons typeName_nosimp func =
  instanceD (cxt [])
    (appT (conT class_name) for_type) 
    [(func_def func)] 
      where func_def (func_name, gen_func) 
                = funD func_name -- method name
                  -- generate function body
                  [gen_clause gen_func cInfo consts typesCons typeName_nosimp]


-- Generate the pattern match and function body for a given method and
-- a given constructor. gen_func is a function that generates the function body
gen_clause :: Gen_func -> [(Name, Int)] -> [Constructor] -> [[Type]] -> Name -> ClauseQ
gen_clause gen_func cInfo consts typesCons typeName_nosimp = 
      (clause []
            (normalB $ head (gen_func cInfo consts listOfFOut (isRec typesCons))) 
             (gen_wheres (map snd cInfo) consts listOfFOut))
      where listOfFOut = listOfF (length consts)
            listOfF 0 = []
            listOfF n = (mkName ("f"++ show n)):(listOfF (n-1))
            isRec [] = []
            isRec (x:xs) = (or $ map (==(ConT typeName_nosimp)) x): isRec xs

            gen_wheres [] [] [] = []
            gen_wheres (n:ns) (c:cs) (f:fs) --numParam consts listOfF 
                | n > 1 = funD f (bodyFunc listOfVar (fst c)):gen_wheres ns cs fs
                | otherwise = gen_wheres ns cs fs
                  where listOfVar = listVariab n
                        listVariab 0 = []
                        listVariab n = (mkName ("x"++ show n)):(listVariab (n-1))

            bodyFunc listOfVar constructorStr = [clause (tupleParam listOfVar)
                                                  (normalB (appsE ((conE constructorStr):
                                                    (map varE listOfVar)))) []]
            tupleParam (v:vs) --listVars
                | (null vs) = [varP v]
                | otherwise = [tupP ((varP v):tupleParam vs)]

            --TODO arreglar saber cuales son recursivas


--Extracting information of the of the declaration of the data type-------
typeInfo :: DecQ -> Q (Name,[(Name, Int)],[(Name, [(Maybe Name, Type)])],[[Type]], Name)
typeInfo m =
     do d <- m
        case d of
           d@(DataD _ _ _ _ _) ->
            return $ (simpleName $ name d, consA d, termsA d, listTypesA d, name d)
           d@(NewtypeD _ _ _ _ _) ->
            return $ (simpleName $ name d, consA d, termsA d, listTypesA d, name d)
           _ -> error ("derive: not a data type declaration: " ++ show d)
 
     where
        consA (DataD _ _ _ cs _)    = map conA cs
        consA (NewtypeD _ _ _ c _)  = [ conA c ]

        --Here we can see if the constructor is Normal, Recursive or Infix
        conA (NormalC c xs)         = (simpleName c, length xs)
        conA (RecC c xs)            = (simpleName c, length xs)
        conA (InfixC _ c _)         = (simpleName c, 2)
 
        nameFromTyVar (PlainTV a)    = a
        nameFromTyVar (KindedTV a _) = a
 
        termsA (DataD _ _ _ cs _)   = map termA cs
        termsA (NewtypeD _ _ _ c _) = [ termA c ]
 
        termA (NormalC c xs)      = (c, map (\x -> (Nothing, snd x)) xs)
        termA (RecC c xs)         = (c, map (\(n, _, t) -> (Just $ simpleName n, t)) xs)
        termA (InfixC t1 c t2)    = (c, [(Nothing, snd t1), (Nothing, snd t2)])
 
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

compose ::[a] -> [b] -> [(a,b)]
compose xs ys = (e:lattice)
  where e:lattice = concat $ diags 0 0 0 xs ys

--
-- It builds the lattice of tuples from two lists, each one may be either
-- finite or infinite


diags :: Int -> Int -> Int -> [a] -> [b] -> [[(a,b)]]
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
                             y = ys !! (i-k)

data MyExp = Const Int Int  | Prod MyExp MyExp | Var Char Char | Sum MyExp MyExp