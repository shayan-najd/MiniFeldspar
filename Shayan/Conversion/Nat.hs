{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses #-}
module Conversion.Nat where

import qualified Data.Nat      as A
import qualified Singleton.Nat as G
import qualified Variable.GADT as V

import Existential
import Conversion

type ExsNat = ExsSin G.Nat 

instance Cnv A.Nat ExsNat where
  cnv A.Zro     = return (ExsSin G.Zro)
  cnv (A.Suc n) = do ExsSin n' <- cnv n
                     return (ExsSin (G.Suc n'))
                     
instance Cnv (V.Var e t) A.Nat where                     
  cnv V.Zro     = return A.Zro
  cnv (V.Suc v) = succ <$> cnv v
