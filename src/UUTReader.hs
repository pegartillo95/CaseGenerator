{-# LANGUAGE TemplateHaskell, ScopedTypeVariables#-}

module UUTReader where

import Control.Monad
import Data.String.Utils
import Language.Haskell.TH
import GHC.Generics
import Sized
import Arbitrary
import TemplateAllv
import TemplateArbitrary
import UUT
import UUTReaderUtilities

{-test_UUT :: Q Bool
test_UUT = do
              strs <- get_f_inp_types "uutPrec"
              names <- strsToNames strs
              userD <- return (isUserDef strs)
              ok <- callGens names userD typesOfGen
              inpList <- getInpList
              sol <- executePreFunPost inpList-}

---------------------One for each type of execution----------------------
{-test_UUT_sized : Q Bool
test_UUT_sized = do
                    strs <- get_f_inp_types "uutPrec"
                    names <- strsToNames strs
                    userD <- return (isUserDef strs)
                    ok <- callGens names userD typesOfGen
                    ------------------MISSING PART TO GET inpList
                    sol <- executePreFunPost inpList

test_UUT_smallest : Q Bool
test_UUT_smallest = do
                       strs <- get_f_inp_types "uutPrec"
                       names <- strsToNames strs
                       userD <- return (isUserDef strs)
                       ok <- callGens names userD typesOfGen
                       ------------------MISSING PART TO GET inpList
                       sol <- executePreFunPost inpList

test_UUT_arbitrary : Q Bool
test_UUT_arbitrary = do
                        strs <- get_f_inp_types "uutPrec"
                        names <- strsToNames strs
                        userD <- return (isUserDef strs)
                        ok <- callGens names userD typesOfGen
                        ------------------MISSING PART TO GET inpList
                        sol <- executePreFunPost inpList-}

--------------generate the test cases-------------------------------------
{-getInpList = $(appsE ((zipN uutNargs):getListFunc))-}

---------------convert list of strings to list of names-------------------
strsToNames :: [String] -> Q [Name]
strsToNames [] = return []
strsToNames (t:ts) = do (Just name) <- lookupValueName t
                        recursive <- strsToNames ts
                        return (name:recursive)

-------------call to gen_all and gen_arbitrary ----------------------------
{-callGens :: [Name] -> [Bool] -> [Int] -> Q Bool
callGens [] [] [] = return True
callGens (n:ns) (d:ds) (t:ts) = do _ <- callSingle n d t
                                   rec <- callGens ns ds ts
                                   return True

callSingle :: Name -> Bool -> Int -> Q Bool
callSingle n d t = if d then do _ <- $(gen_arbitrary n)
                                _ <- $(gen_allv n)
                   return True-}

---------------Discover user defined types -----------------------------
isUserDef :: [String] -> [Bool]
isUserDef [] = []
isUserDef (t:ts) = (userDef t):(isUserDef ts)

userDef :: String -> Bool
userDef str = if((lastMod str "" "")   == "UUT") then True
               else False
lastMod :: String -> String -> String -> String
lastMod [] lastAcc sol = sol
lastMod (t:ts) lastAcc sol
     | t == '.' = lastMod ts "" lastAcc
     | otherwise = lastMod ts (lastAcc ++ [t]) sol

----------------Execute precondition, function, postcondition ----------

executePreFunPost listInp = (return (filterPrec listInp []) >>=
                              (\list -> return (passFun list) >>=
                                (\(x,y) -> testPost x y
                                  )))

----------------Functions to filter the list of cases by precondition---

filterPrec [] solList = solList 
filterPrec (t:ts) solList = filterPrec ts (solList++x)
     where x = if (testPrec t) then [t]
               else []

testPrec t = $(lamE (tupleParam lVar) (body "uutPrec" lVar)) t

        
---------Appplying the function to get the corresponding outputs---------
passFun list = (list, (passFunAux list))

passFunAux [] = []
passFunAux (t:ts)= y:(passFunAux ts)
     where y = testFun t

testFun t = $(lamE (tupleParam lVar) (body "uutMethod" lVar)) t


-----Function to test the postcondition----------------------------------
testPost [] [] = putStrLn "Test finished correctly"
testPost (t:ts) (o:os) = do
                                  when (not aux_bool_q) $ do
                                        putStr ("Failed on function " ++ uutName ++ " with inputs ")
                                        print ts
                                        putStr " and output "
                                        print os
                                        putStrLn ""
                                  testPost ts os
                                  where aux_bool_q = post t o

post t o = $(lamE ((tupleParam lVar)++[varP nameY]) (body2 "uutPost" lVar)) t o