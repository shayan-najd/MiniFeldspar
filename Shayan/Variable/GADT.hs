{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, StandaloneDeriving, PolyKinds #-}
{-# LANGUAGE TypeOperators, DataKinds, PolyKinds #-}
module Variable.GADT where

import qualified Data.Nat as N

-- Variables
data Var :: [k] -> k -> * where
  Zro :: Var (t ': r) t
  Suc :: Var r tp -> Var (t ': r) tp

deriving instance Eq (Var e t)
  
instance Show (Var e t) where
 show = ("x"++) . show . cnvNat

cnvNat :: Var e t -> N.Nat  
cnvNat Zro     =  N.Zro
cnvNat (Suc v) =  succ (cnvNat v) 
 