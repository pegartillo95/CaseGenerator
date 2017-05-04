{-# LANGUAGE TemplateHaskell #-}

module UUTReader where

import Language.Haskell.TH
import GHC.Generics
import Sized
import Arbitrary
import TemplateAllv
import TemplateArbitrary
--import de la clase de UUT


--reads from the UUT archive the number of methods to tests 
--them one by one.
{-read_UUT = test_UUT uutNMethods

test_UUT n
   | n > 0 = do
   	           reified <- reify_func ("uutPrec-" ++ (show n))
               info <- extract_info reified



reify_func string = $(lookupValueName string >>= 
                        (\(Just name) -> reify name))

extract_info inform = 
	      do i <- inform
              case i of
                d@(VarI _ _ _ _) ->
                  return $ (funcName, simpleName $ funcName)
                d@(ClassOpI _ _ _ _) ->
                  return $ (funcName, simpleName $ funcName)
              _ -> error ("derive: not a function declaration: " ++ show d)
           where 
           	  funcName (VarI n _ _ _)  = n
           	  funcName (ClassOpI n _ _ _) = n

           	  inputParTypes (VarI _ xs _ _) = map getType xs

           	  getType (KindedTV a _) = a


simpleName :: Name -> Name
simpleName nm =
   let s = nameBase nm
   in case dropWhile (/=':') s of
        []          -> mkName s
        _:[]        -> mkName s
        _:t         -> mkName t-}

