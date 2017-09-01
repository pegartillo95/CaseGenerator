{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH
import GHC.Generics
import Sized
import Arbitrary
import TemplateAllv
import TemplateArbitrary
import UUTReader
import UUT
import UUTReaderUtilities
import System.IO.Unsafe

-----------------------------------------------------------------------------------
-- | Programming generic size and generic enumeration of values
-- | autor: Ricardo Peña & Pedro García Castillo, febrero 2017
-----------------------------------------------------------------------------------

--Dumb main function
main :: IO ()
main = putStrLn $(get_f_inp_types (head uutMethods) >>=
                   (\x -> stringE $ show x
                      ))

{-main = putStrLn $(lookupValueName "uutPrec" >>=
                    (\(Just name) -> extract_info (reify name) >>=
                       (\(_,_,t) -> return(simplifyParsing t) >>=
	                      (\text -> stringE $ show text
	                        ))))-}

{-main = putStrLn $(test_UUT >>=
                   (\x -> stringE $ printInfo x
                     ))-}

--VarI UUT.uutPrec (ForallT [KindedTV t_1627411922 StarT,KindedTV a_1627411923 StarT] [AppT (ConT GHC.Classes.Ord) (VarT a_1627411923)] (AppT (AppT ArrowT (VarT t_1627411922)) (AppT (AppT ArrowT (ConT GHC.Types.Int)) (AppT (AppT ArrowT (AppT (ConT Arrays.Array) (VarT a_1627411923))) (ConT GHC.Types.Bool))))) Nothing (Fixity 9 InfixL)