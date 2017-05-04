{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH
import GHC.Generics
import Sized
import Arbitrary
import TemplateAllv
import TemplateArbitrary

-----------------------------------------------------------------------------------
-- | Programming generic size and generic enumeration of values
-- | autor: Ricardo Peña & Pedro García Castillo, febrero 2017
-----------------------------------------------------------------------------------

--Dumb main function
main :: IO ()
main = putStrLn $(lookupValueName "gen_arbitrary" >>= 
                  (\(Just name) -> reify name >>= 
                       (\info -> (stringE . show) info)))
--main = putStrLn $ pprint ($(doE [letS [(gen_arbitrary ''MyExp)]]))
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

{-VarI GHC.Base.map
      (ForallT [KindedTV a_1627410524 StarT,KindedTV b_1627410525 StarT]
        []
         (AppT (AppT ArrowT
               (AppT (AppT ArrowT (VarT a_1627410524)) (VarT b_1627410525))
               ) 
         (AppT (AppT ArrowT (AppT ListT (VarT a_1627410524))) (AppT ListT (VarT b_1627410525)))
         )
      ) 
      Nothing (Fixity 9 InfixL)-}

{-ClassOpI GHC.Real.div
  (ForallT [KindedTV a_1627411077 StarT]
     [AppT (ConT GHC.Real.Integral) (VarT a_1627411077)] 
      (AppT (AppT ArrowT (VarT a_1627411077)) (AppT (AppT ArrowT (VarT a_1627411077)) (VarT a_1627411077)))) 
      GHC.Real.Integral (Fixity 7 InfixL)-}
 
