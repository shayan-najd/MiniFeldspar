{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs,TypeFamilies,FlexibleInstances #-}
module Expression.Feldspar.MiniWellScoped where

import Data.Array
import Variable.GADT

data Exp r t where
  ConI  :: Integer -> Exp r Integer
  ConB  :: Bool -> Exp r Bool
  Cnd   :: Exp r Bool -> Exp r t -> Exp r t -> Exp r t
  Whl   :: (Exp r s -> Exp r Bool) -> (Exp r s -> Exp r s) -> Exp r s -> Exp r s
  Tpl   :: Exp r t -> Exp r b -> Exp r (t , b)
  Fst   :: Exp r (t , b) -> Exp r t
  Snd   :: Exp r (t , b) -> Exp r b
  Ary   :: Exp r Integer -> (Exp r Integer -> Exp r t) -> Exp r (Array Integer t)
  Len   :: Exp r (Array Integer t) -> Exp r Integer
  Ind   :: Exp r (Array Integer t) -> Exp r Integer -> Exp r t
  Let   :: Exp r t -> ( t -> Exp r b) -> Exp r b
  Var   :: Var r t -> Exp r t 
  Prm1  :: Var r (ta -> tb) -> Exp r ta -> Exp r tb
  Prm2  :: Var r (ta -> tb -> tc) -> Exp r ta -> Exp r tb -> Exp r tc
  Val   :: t -> Exp r t
  Undef :: Exp r t 
  
   
