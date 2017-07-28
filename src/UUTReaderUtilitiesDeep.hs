{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module UUTReaderUtilitiesDeep where

import Control.Monad
import Language.Haskell.TH
import UUTReaderUtilitiesDeeper
import UUT
import Sized

--------------Create a list of variables -----------------------------
listVar :: Int -> [Name]
listVar 0 = []
listVar n = (mkName ("x"++ show n)):(listVar (n-1))

-------------Linked tuples creator-------------------------------------
linkedTupleP :: Int -> PatQ
linkedTupleP 1 = varP (mkName ("x"++ (show 1)))
linkedTupleP n = tupP ((varP (mkName ("x"++ (show n)))):[(linkedTupleP (n-1))])

-------------zip depending on the number of input params------------------
zipN_f = $(zipN)

------------------generate test function--------------------------------------
genTest :: Q [Dec]
genTest = do
               prueba <- return (mkName "prueba")
               listArg <- return (mkName "listArg")
               test <- testN prueba listArg
               wherecl <- funD listArg [clause [] (normalB build_where) []]
               args <- (varP listArg)
               let name = mkName "test"
               return [FunD name [Clause [] (NormalB test) [wherecl]]]

testN :: Name -> Name -> ExpQ
testN pName listArg = do appsE (map varE (pName:[listArg]))

build_where :: ExpQ
build_where = appsE [varE 'smallest]
--appsE ((varE 'zipN_f):(take uutNargs (repeat(appsE [varE 'smallest])))) PREVIOUS VERSION