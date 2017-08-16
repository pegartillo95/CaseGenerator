{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module UUTReaderUtilitiesDeep where

import Control.Monad
import Language.Haskell.TH
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