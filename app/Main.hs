{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH
import GHC.Generics
import Sized
import TemplateAllv

-----------------------------------------------------------------------------------
-- | Programming generic size and generic enumeration of values
-- | autor: Ricardo Peña & Pedro García Castillo, febrero 2017
-----------------------------------------------------------------------------------

--Dumb main function
main :: IO ()
main = putStrLn $ pprint ($(doE [letS [(gen_allv ''MyExp)]]))
--main =putStrLn $(stringE . show =<< reify ''Bolean)      

