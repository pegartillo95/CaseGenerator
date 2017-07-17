{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module UUTReaderUtilitiesDeep where

import Control.Monad
import Language.Haskell.TH
import UUT

zipN :: ExpQ
zipN = [| let zp = $(mkZip uutNargs [| zp |]) in zp |]

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
        m2 = match (tupP (take n (repeat wildP))) (normalB (conE $ mkName "[]")) []

listNames :: String -> Int -> [Name]
listNames x n = reverse (listNamesAux x n)

listNamesAux :: String -> Int -> [Name]
listNamesAux x 0 = []
listNamesAux x n = (mkName (x++(show n))):(listNamesAux x (n-1))

--------------Create a list of variables -----------------------------
listVar :: Int -> [Name]
listVar 0 = []
listVar n = (mkName ("x"++ show n)):(listVar (n-1))