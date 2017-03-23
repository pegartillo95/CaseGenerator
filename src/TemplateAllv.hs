{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts, TemplateHaskell #-}

module TemplateAllv (
    MyExp(..)
    , gen_allv
    , compose
    )where

import Language.Haskell.TH
import Data.Char

data MyExp = Const Int | Var Char | Sum MyExp MyExp | Prod MyExp MyExp

-- Generate an intance of the class TH_Render for the type typName
gen_allv :: Name -> Q Dec
gen_allv typName =
  do (TyConI d) <- reify typName -- Get all the information on the type
     (type_name,_,_,constructors,kindOfCons) <- typeInfo (return d) -- extract name and constructors                  
     i_dec <- gen_instance (mkName "Allv") (conT type_name) constructors kindOfCons
                      -- generation function for method "render"
                      [(mkName "allv", gen_allv)]
     return i_dec -- return the instance declaration
             -- function to generation the function body for a particular function
             -- and constructor
       where gen_allv (conName, components) vars 
                 -- function name is based on constructor name  
               = let funcName = makeName $ nameBase conName 
                 -- choose the correct builder function
                     headFunc = case vars of
                                     [] -> "no_arg" --Enter when no arguments
                                     otherwise -> "map" --Enters when the dataType Constructor do have arguments
                      -- build 'funcName parm1 parm2 parm3 ...
                   in appsE $ (varE $ mkName headFunc):funcName:vars -- put it all together
             -- equivalent to 'funcStr where funcStr CONTAINS the name to be returned
             makeName funcStr = (appE (varE (mkName "mkName")) (litE $ StringL funcStr))

type Constructor = (Name, [(Maybe Name, Type)]) -- the list of constructors
type Cons_vars = [ExpQ] -- A list of variables that bind in the constructor
type Function_body = ExpQ 
type Gen_func = Constructor -> Cons_vars -> Function_body
type Func_name = Name   -- The name of the instance function we will be creating
-- For each function in the instance we provide a generator function
-- to generate the function body (the body is generated for each constructor)
type Funcs = [(Func_name, Gen_func)]

-- construct an instance of class class_name for type for_type
-- funcs is a list of instance method names with a corresponding
-- function to build the method body
gen_instance :: Name -> TypeQ -> [Constructor] -> [Int] -> Funcs -> DecQ
gen_instance class_name for_type constructors kindOfCons funcs = 
  instanceD (cxt [])
    (appT (conT class_name) for_type) 
    (map func_def funcs) 
      where func_def (func_name, gen_func) 
                = funD func_name -- method name
                  -- generate function body for each constructor
                  --(map (gen_clause gen_func) constructors)
                  [gen_clause gen_func constructors]


-- Generate the pattern match and function body for a given method and
-- a given constructor. func_body is a function that generations the
-- function body
--gen_clause :: (Constructor -> [ExpQ] -> ExpQ) -> Constructor -> ClauseQ
--gen_clause func_body data_con@(con_name, components) = 
gen_clause :: (Constructor -> [ExpQ] -> ExpQ) -> [Constructor] -> [Int] -> ClauseQ
gen_clause func_body constructors kindOfCons = 
      -- create a parameter for each component of the constructor
   --do vars <- mapM var components
   do vars <- mapM var components
      -- function (unnamed) that pattern matches the constructor 
      -- mapping each component to a value.
      (clause []
            (normalB (func_body data_con (map varE vars))) -- This is the function we define in gen_allv
             []) -- declarations of the type where
       -- create a unique name for each component. 
       where var (_, typ) 
                 = newName 
                   $ case typ of 
                     (ConT name) -> toL $ nameBase name
                     otherwise   -> "parm"
               where toL (x:y) = (toLower x):y
 
unCapalize :: [Char] -> [Char]
unCapalize (x:y) = (toLower x):y


--Extracting information of the of the declaration of the data type-------
typeInfo :: DecQ -> Q (Name, [Name], [(Name, Int)], [(Name, [(Maybe Name, Type)])],[Int])
typeInfo m =
     do d <- m
        case d of
           d@(DataD _ _ _ _ _) ->
            return $ (simpleName $ name d, paramsA d, consA d, termsA d, typeConsA d)
           d@(NewtypeD _ _ _ _ _) ->
            return $ (simpleName $ name d, paramsA d, consA d, termsA d, typeConsA d)
           _ -> error ("derive: not a data type declaration: " ++ show d)
 
     where
        consA (DataD _ _ _ cs _)    = map conA cs
        consA (NewtypeD _ _ _ c _)  = [ conA c ]

        --Here we can see if the constructor is Normal, Recursive or Infix
        conA (NormalC c xs)         = (simpleName c, length xs)
        conA (RecC c xs)            = (simpleName c, length xs)
        conA (InfixC _ c _)         = (simpleName c, 2)
 
        paramsA (DataD _ _ ps _ _) = map nameFromTyVar ps
        paramsA (NewtypeD _ _ ps _ _) = map nameFromTyVar ps
 
        nameFromTyVar (PlainTV a) = a
        nameFromTyVar (KindedTV a _) = a
 
 
        termsA (DataD _ _ _ cs _) = map termA cs
        termsA (NewtypeD _ _ _ c _) = [ termA c ]
 
        termA (NormalC c xs)        = (c, map (\x -> (Nothing, snd x)) xs)
        termA (RecC c xs)           = (c, map (\(n, _, t) -> (Just $ simpleName n, t)) xs)
        termA (InfixC t1 c t2)      = (c, [(Nothing, snd t1), (Nothing, snd t2)])
 
        name (DataD _ n _ _ _)      = n
        name (NewtypeD _ n _ _ _)   = n
        name d                      = error $ show d

        typeConsA (DataD _ _ _ cs _)    = map fromIntegral (map tyConA cs)
        typeConsA (NewtypeD _ _ _ c _)  = map fromIntegral [ tyConA c ]

        tyConA (NormalC _ _)         = 0
        tyConA (RecC _ _)            = 1
        tyConA (InfixC _ _ _)         = 2
 
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