{-# LANGUAGE TemplateHaskell #-}

module TemplateArbitrary (
    MyExp(..)
    , gen_arbitrary
    )where

import Language.Haskell.TH
import Data.Char
import System.Random

import Control.Monad
  ( liftM
  , liftM2
  , liftM3
  , liftM4
  , liftM5
  )



data MyExp = Const Int | Prod MyExp MyExp | Var Char | Sum MyExp MyExp


------------------------------------------------------------------------------------
-------------------- Usefull alias for some data types------------------------------
------------------------------------------------------------------------------------

-- the list of consts
type Constructor = (Name, [(Maybe Name, Type)])
-- A list of variables that bind in the constructor
type Cons_vars = [ExpQ]
--The type of the function that generates the body
type Gen_func = Name -> [ExpQ]
-- The name of the instance function we will be creating
type Func_name = Name   
--Tuple that pairs the func_name and the function to generate the body
type Func = (Func_name, Gen_func)

-- Generate an intance of the class Arbitrary for the type typName
gen_arbitrary :: Name -> Q Dec
gen_arbitrary typName =
  do (TyConI d) <- reify typName
      --Extract all the type info of the data type 
     (t_name,noSimplifiedName,cInfo,consts,typesCons) <- typeInfo (return d)
     --We call to gen_instance with a name for the class, the name of the constructor,
     --a list of info of the constructors, the constructors itself, lists containing
     --the constructors, name of the data-type without being simplified, and lastly
     --the function to generate the body of the function of the class.            
     i_dec <- gen_instance (mkName "Arbitrary") (conT t_name) cInfo consts
                        typesCons noSimplifiedName t_name (mkName "arbitrary", gen_body)
     return i_dec -- return the instance declaration
            -- gen_body is the function that we pass as an argument to gen_instance
            --and later on is used to generate the body of the arbitrary function 
            --for a determined data-type
       where gen_body :: Name -> [ExpQ]
             gen_body typeName = [appE sizedE typeNameE]
                      where 
                            typeNameE = varE typeName
                            sizedE = functE "sized"
                            functE string = varE $ mkName string


--Construct an instance of class class_name for type for_type
--with a corresponding function  to build the method body
gen_instance :: Name -> TypeQ -> [(Name, Int)] -> [Constructor]
                  -> [[Type]] -> Name -> Name -> Func -> DecQ
gen_instance class_name for_type cInfo consts typesCons typeName_nosimp t_name func =
  instanceD (cxt [])
    (appT (conT class_name) for_type) 
    [(func_def func)] 
      where func_def (func_name, gen_func)-- extracts func_name and gen_func
                = funD func_name -- method name
                  -- generate function body
                  [gen_clause gen_func cInfo consts typesCons typeName_nosimp t_name]


-- Generate the pattern match and function body for a given method and
-- a given data-type. gen_func is the function that generates the function body
gen_clause :: Gen_func -> [(Name, Int)] -> [Constructor] 
                 -> [[Type]] -> Name -> Name -> ClauseQ
gen_clause gen_func cInfo consts typesCons typeName_nosimp t_name = 
      (clause []
             --here we execute the gen_function to generate the body of the function
            (normalB $ head (gen_func typeName))
             --this other one generates the where clause of the function
            (gen_wheres typeName isRecOrd isRecPerParamOrd numParamOrd constsOrd))
      where 
            --these are getters for the reordered version of the 4 lists.
            isRecOrd = (\(x,_,_,_) -> x) reorderL
            isRecPerParamOrd = (\(_,x,_,_) -> x) reorderL
            numParamOrd = (\(_,_,x,_) -> x) reorderL
            constsOrd = (\(_,_,_,x) -> x) reorderL

            --reorders all 3 list taking the non recursive to the begining
            reorderL = auxFirst isRec isRecPerParam numParam consts 0 False
            auxFirst rs ps ns cs n foundRec
                |n > ((length rs)-1) = (rs, ps, ns, cs)
                |foundRec && (not (rs!!n)) = auxFirst 
                                              ((rs!!n):(remove n rs 0)) 
                                              ((ps!!n):(remove n ps 0)) 
                                              ((ns!!n):(remove n ns 0))
                                              ((cs!!n):(remove n cs 0)) 0 False
                |(not foundRec) && (rs!!n) = auxFirst rs ps ns cs (n+1) (not foundRec)
                |otherwise = auxFirst rs ps ns cs (n+1) foundRec
            --removes position n from the list (x:xs)
            remove n (x:xs) actPos
                |n==actPos = xs
                |otherwise = x:(remove n xs (actPos+1))

            --returns the equivalent to t_name but of type Name and the first letter
            --in lower case
            typeName = mkName $ toLowerFirst $ nameBase t_name
            toLowerFirst (x:xs) = (toLower x):xs
            --isRec checks which of the constructors of the given data-type
            --are recursive and which others are not. It returns a boolean list
            --where true means to be recursive and false to not to be recursive.
            isRec = isRecAux typesCons
            isRecAux [] = []
            isRecAux (x:xs) = (or $ map (==(ConT typeName_nosimp)) x): isRecAux xs
            --returns a list of list each of the inner list containing if each of the
            --parameters of that constructor is recursive or not
            isRecPerParam = recPerParAux typesCons
            recPerParAux [] = []
            recPerParAux (x:xs) = (map (==(ConT typeName_nosimp)) x): recPerParAux xs
            --list of number of params of each of the constructors
            numParam = map length typesCons   
            
            --gen_wheres is the auxiliar function that generates the where "clause"
            --of the function when necesary.
            gen_wheres _ [] [] [] [] = []
            gen_wheres t_n rs ps ns cs = [funD t_n [clause [nvarP]
                                              --body of the myExp function
                                             (bodyFunc nName fName rs ps ns (map fst cs))
                                              --inner where of the myExp function
                                              [funD fName [clause [] (normalB
                                               (appE (varE t_n) (appsE (divE:nFunE:[[e|2|]])))) []]
                                              ]]]
               where 
                    nName = mkName "n"
                    nFunE = varE nName
                    fName = mkName "myExp2"
                    divE = varE 'div
                    nvarP = varP $ nName
                    

            bodyFunc nName fName rs ps ns cs = normalB (appsE 
                                                (freqE:[freqL nName fName rs ps ns cs])) 
                                          
              where
                    freqE = varE $ mkName "frequency"--TODO change to varE 'frequency
                   

            freqL nName fName rs ps ns cs
               | isNormal && isRecur = appsE (concE:(listE 
                                          (normalAux fName (fst indices) (beg ps nNor) (beg ns nNor) (beg cs nNor))):
                                            [compE [noBindS (appsE (greaterE:nVarE:[[e|0|]])),noBindS (appsE (concatE:[recurAux nName fName (snd indices) (end ps nNor) (end ns nNor) (end cs nNor)]))]])
               | isNormal = listE (normalAux fName (fst indices) (beg ps nNor) (beg ns nNor) (beg cs nNor))
               | isRecur = appsE (concatE:(recurAux nName fName (snd indices) (end ps nNor) (end ns nNor) (end cs nNor)):[compE [noBindS (appsE (greaterE:nVarE:[[e|0|]]))]])

                where 
                      concE = varE '(++)
                      greaterE = varE '(>)
                      nVarE = varE nName
                      concatE = varE 'concat
                      indices = (eraseZeros (2000 `div` nNor), eraseZeros (8000 `div` nRec)) --I'm giving a 20% to base case and 80% to recursive
                      eraseZeros n
                         | n > 9 = eraseZeros (n `div` 10)
                         | otherwise = n
                      isRecur = nRec > 0
                      nRec = length $ filter (==True) rs
                      isNormal =  nNor > 0
                      nNor = length $ filter (==False) rs
                      beg (x:xs)  n
                         | n == 0 = []
                         | otherwise = x:(beg xs (n-1))
                      end (x:xs) n
                         | n == 0 = (x:xs)
                         | otherwise = end xs (n-1)

            normalAux _ _ [] [] [] = []
            normalAux fName num (p:ps) (n:ns) (c:cs) = (tupE (([e|num|]):[appsE ((selectlift n):(conE c):(arbitraryArg fName p))])):(normalAux fName num ps ns cs)
            recurAux nName fName n ps ns cs = listE [listE (recAux2 fName n ps ns cs)]
            recAux2 _ _ [] [] [] = []
            recAux2 fName num (p:ps) (n:ns) (c:cs) = (tupE (([e|num|]):[appsE ((selectlift n):(conE c):(arbitraryArg fName p))])):(recAux2 fName num ps ns cs)

            arbitraryArg _ [] = []
            arbitraryArg fName (x:xs)
              | x == False = (arbitraryE):(arbitraryArg fName xs)
              | x == True = (fExp):(arbitraryArg fName xs)

                  where
                       arbitraryE = varE $ mkName "arbitrary" --TODO change to varE 'arbitrary
                       fExp = varE fName
                    
            selectlift n 
              | n == 1 = liftME
              | n == 2 = liftM2E
              | n == 3 = liftM3E
              | n == 4 = liftM4E
              | n == 5 = liftM5E
                where 
                     liftME = varE 'liftM
                     liftM2E = varE 'liftM2
                     liftM3E = varE 'liftM3
                     liftM4E = varE 'liftM4
                     liftM5E = varE 'liftM5



--Extracting information of the of the declaration of the data type, these are:
--  > The name of the type simplified (without the module name)
--  > The name of the type without being simplified
--  > A list of tuples one for each constructor having the name and the number
--      of arguments of each of them.
--  > A list of a tuple having the name of the type constructor and a list of maybe
--      the data constructor and the type of its parameters. 
--  > List of lists each of the inner list having the types of the parameters of
--      data constructor.
typeInfo :: DecQ -> Q (Name, Name,[(Name, Int)],[(Name, [(Maybe Name, Type)])],[[Type]])
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

--TyConI (DataD [] TemplateArbitrary.MyExp [] [
--NormalC TemplateArbitrary.Const [(NotStrict,ConT GHC.Types.Int),(NotStrict,ConT GHC.Types.Int)],
--NormalC TemplateArbitrary.Prod [(NotStrict,ConT TemplateArbitrary.MyExp),(NotStrict,ConT TemplateArbitrary.MyExp)],
--NormalC TemplateArbitrary.Var [(NotStrict,ConT GHC.Types.Char),(NotStrict,ConT GHC.Types.Char)],
--NormalC TemplateArbitrary.Sum [(NotStrict,ConT TemplateArbitrary.MyExp),(NotStrict,ConT TemplateArbitrary.MyExp)]] [])


------------------------------------------
--------Auxiliary types-------------------
------------------------------------------

-- | The "standard" QuickCheck random number generator.
-- A wrapper around either 'TFGen' on GHC, or 'StdGen'
-- on other Haskell systems.
{-newtype QCGen = QCGen TheGen

instance Show QCGen where
  showsPrec n (QCGen g) = showsPrec n g
instance Read QCGen where
  readsPrec n xs = [(QCGen g, ys) | (g, ys) <- readsPrec n xs]

instance RandomGen QCGen where
  split (QCGen g) =
    case split g of
      (g1, g2) -> (QCGen g1, QCGen g2)
  genRange (QCGen g) = genRange g
  next (QCGen g) =
    case next g of
      (x, g') -> (x, QCGen g')-}



------------------------------------------
--------Auxiliary functions---------------
------------------------------------------

-- | A generator for values of type @a@.
{-newtype Gen a = MkGen{
  unGen :: QCGen -> Int -> a -- ^ Run the generator on a particular seed.
 }

frequency :: [(Int, Arbitrary a)] -> Arbitrary a
frequency [] = error "QuickCheck.frequency used with empty list"
frequency xs0 = choose (1, tot) >>= (`pick` xs0)
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error "QuickCheck.pick used with empty list"


sized :: (Int -> Gen a) -> Gen a
sized f = MkGen (\r n -> let MkGen m = f n in m r n)

-- | Generates a random element in the given inclusive range.
choose :: Random a => (a,a) -> Gen a
choose rng = MkGen (\r _ -> let (x,_) = randomR rng r in x)-}
