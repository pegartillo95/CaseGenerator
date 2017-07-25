{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE GADTs #-}

module Asertos where

import qualified Sets as Set
import qualified Sequences as Seq
import qualified Arrays as A
import qualified Bags as B
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Data.Ix

----------------------------------------------------------------------------
-- * Data type for Why3 types
-- | 

class Why3Type a where

instance Why3Type Int

instance Why3Type Float

instance Why3Type a => Why3Type [a]


-- Generalised Algebraic Data Type for assertions

data Assert a where
   FTerm  :: Term Bool -> Assert a
   And    :: Assert a -> Assert a -> Assert a
   Or     :: Assert a -> Assert a -> Assert a
   Imp    :: Assert a -> Assert a -> Assert a    -- A  -> B
   Equiv  :: Assert a -> Assert a -> Assert a    -- A <-> B
   Not    :: Assert a -> Assert a
   Forall :: Guard b -> (b -> Assert b) -> Assert b
   Exists :: Guard b -> (b -> Assert b) -> Assert b

-- Generalised Algebraic Data Type for guards

data Guard a where
   GuardInt      :: Term Int -> Term Int -> Guard Int
   GuardIntTuple :: Term (Int,Int) -> Term (Int,Int) -> Guard (Int,Int)
   GuardSet      :: Term (Set.Set a) -> Guard a
   GuardSeq      :: Term (Seq.Seq a) -> Guard a
   GuardBag      :: Term (B.Bag a)   -> Guard a

-- Generalised Algebraic Data Type for terms

data Term a where
    TConst :: a -> Term a
    TVar   :: a -> Term a
    TIf    :: Term Bool -> Term a -> Term a -> Term a
    TLet   :: Term a -> (a -> Term b) -> Term b
    Aplic  :: Term (a -> b) -> Term a -> Term b
    Tuple2 :: Term a -> Term b -> Term (a,b)
    Tuple3 :: Term a -> Term b -> Term c -> Term (a,b,c)

-- evaluating terms
evalT :: Term a -> a

evalT (TConst c)        = c
evalT (TVar x)          = x
evalT (TIf tb t1 t2)    = if evalT tb then evalT t1 else evalT t2
evalT (TLet t f)        = evalT $ f (evalT t)
evalT (Aplic tf ta)     = evalT tf $ evalT ta
evalT (Tuple2 t1 t2)    = (evalT t1, evalT t2)
evalT (Tuple3 t1 t2 t3) = (evalT t1, evalT t2, evalT t3)

-- evaluating assertions
evalA :: Assert a -> Bool

evalA (FTerm t)     = evalT t
evalA (And a1 a2)   = evalA  a1 && evalA a2
evalA (Or a1 a2)    = evalA a1 || evalA a2
evalA (Imp a1 a2)   = (not $ evalA a1) || evalA a2
evalA (Equiv a1 a2) = evalA a1 == evalA a2
evalA (Not a)       = not $ evalA a
evalA (Forall g f)  = and $ map (evalA . f) (domain g)
evalA (Exists g f)  = or  $ map (evalA . f) (domain g)

-- domain function
domain::Guard a -> [a]

domain (GuardInt min max)      = [evalT min .. evalT max]
domain (GuardIntTuple min max) = range (evalT min,evalT max)
domain (GuardSet s)            = S.toList $ evalT s
domain (GuardSeq s)            = evalT s
domain (GuardBag b)            = M.keys $ evalT b