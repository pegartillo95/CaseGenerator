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
main = putStrLn $(stringE $ lastMod "Hola.Nuevo.Adios.Type" "" "")
{-main = do e <- runQ (getInpProve)
          print e-}


{-main = print (unsafePerformIO (getWithMessage uutNargs))-}



{-main = putStrLn $(lookupValueName "tard" >>= 
                  (\(Just name) -> extract_info (reify name) >>=
                           (\(_,_,text) -> return(simplifyParsing text) >>=
                                  (\t -> return(extract_types t [] "") >>=
                                     (\(x:x2:xs) -> stringE x2
                                       )))))-}

{-main = putStrLn $(lookupValueName "tard" >>= 
                  (\(Just name) -> extract_info (reify name) >>=
                           (\(_,_,text) -> stringE text
                            )))-}



{-main = putStrLn $(lookupValueName "stupid" >>=
                  (\(Just name) -> reify name >>=
                     (\info -> (stringE . show) info
                    )))-}

--main = putStrLn $ pprint ($(doE [letS [(gen_allv ''MyExp)]]))
--main =putStrLn $(stringE . show =<< reify ''MyExp)

{-VarI TemplateAllv.compose 
     (ForallT [KindedTV a_1627407710 StarT,KindedTV b_1627407711 StarT] 
              [] 
              (AppT (AppT ArrowT 
                          (AppT ListT (VarT a_1627407710))
                          ) 
                    (AppT (AppT ArrowT 
                                (AppT ListT (VarT b_1627407711))
                                ) 
                          (AppT ListT 
                                (AppT (AppT (TupleT 2) 
                                            (VarT a_1627407710)
                                            ) 
                                      (VarT b_1627407711)
                                      )
                                )
                          )
              )
     ) 
     Nothing 
     (Fixity 9 InfixL)-}


