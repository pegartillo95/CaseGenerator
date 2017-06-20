{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH
import GHC.Generics
import Sized
import Arbitrary
import TemplateAllv
import TemplateArbitrary
import UUTReader

-----------------------------------------------------------------------------------
-- | Programming generic size and generic enumeration of values
-- | autor: Ricardo Peña & Pedro García Castillo, febrero 2017
-----------------------------------------------------------------------------------

--Dumb main function
main :: IO ()
main = putStrLn $(lookupValueName "compose" >>= 
                  (\(Just name) -> extract_info (reify name) >>=
                           (\(_,_,text) -> stringE $ simplifyParsing text
                            )))


{-main = putStrLn $(lookupValueName "extract_info" >>= 
                  (\(Just name) -> reify name >>= 
                       (\info -> (stringE . show) info
                        )))-}
main = putStrLn $ pprint ($(doE [letS [(gen_allv ''MyExp)]]))
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


{-VarI TemplateArbitrary.gen_arbitrary
 (AppT (AppT ArrowT (ConT Language.Haskell.TH.Syntax.Name)) 
 (AppT (ConT Language.Haskell.TH.Syntax.Q) (ConT Language.Haskell.TH.Syntax.Dec)))
  Nothing (Fixity 9 InfixL)-}

{-VarI UUTReader.extract_info
 (AppT (AppT ArrowT (ConT Language.Haskell.TH.Lib.InfoQ))
  (AppT (ConT Language.Haskell.TH.Syntax.Q)
   (AppT (AppT (AppT (TupleT 3) (ConT Language.Haskell.TH.Syntax.Name))
    (ConT Language.Haskell.TH.Syntax.Name)) (ConT GHC.Base.String))))
     Nothing (Fixity 9 InfixL)-}
 
