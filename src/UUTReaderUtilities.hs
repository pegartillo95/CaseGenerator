{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module UUTReaderUtilities where

import Control.Monad
import Language.Haskell.TH
import TemplateAllv
import Sized
import Arbitrary
import Data.String.Utils
import System.IO.Unsafe
import UUT

----------Recognize user defined types --------------------------------
notDefTypes :: [String] -> [String]
notDefTypes [] = []
notDefTypes (x:xs)
   | (isUserDef (plain x)) = (plain x):(notDefTypes xs)
   | otherwise = notDefTypes xs

isUserDef :: String -> Bool
isUserDef str
   | str == "Int" || str == "Char" || str == "Bool" = True
   | otherwise = False

plain :: String -> String
plain [] = []
plain (x:xs)
  | x == "[" || x == "]" = plain xs
  | otherwise = x:(plain xs)


-------------zip depending on the number of input params------------------
{-zipN :: Int -> ExpQ
zipN n = [| let zp = $(mkZip n [| zp |]) in zp |]

mkZip ::Int -> ExpQ -> ExpQ
mkZip n name = lamE pYs (caseE (tupE eYs) [m1,m2])
      where
        pXs = map varP (listNames "x" n)
        eXs = map varE (listNames "x" n)
        pYs = map varP (listNames "y" n)
        eYs = map varE (listNames "y" n)
        pXSs = map varP (listNames "xs" n)
        eXSs = map varE (listNames "xs" n)
        pcons x xs = [p|$x : $xs|]
        b = [| $(tupE eXs) : $(appsE(name : eXSs))|]
        m1 = match (tupP (zipWith pcons pXs pXSs)) (normalB b) []
        m2 = match (tupP (take n (repeat wildP))) (normalB (conE $ mkName "[]")) []-}