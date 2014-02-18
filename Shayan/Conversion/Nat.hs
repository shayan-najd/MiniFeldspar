{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs, ImplicitParams, MultiParamTypeClasses #-}
module Conversion.Nat where

import qualified Data.Nat as A
import qualified Singleton.Nat  as G

import Existential
import Conversion

type ExsNat = ExsSin G.Nat 

instance Cnv A.Nat ExsNat where
  cnv A.Zro     = return (ExsSin G.Zro)
  cnv (A.Suc n) = do ExsSin n' <- cnv n
                     return (ExsSin (G.Suc n'))