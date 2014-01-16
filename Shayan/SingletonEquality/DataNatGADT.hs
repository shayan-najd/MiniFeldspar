{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module SingletonEquality.DataNatGADT where

import Data.Nat.GADT 
import SingletonEquality

instance EqlSin Nat where
  eqlSin Zro     Zro      = return Rfl
  eqlSin (Suc n) (Suc n') = do Rfl <- eqlSin n n' 
                               return Rfl 
  eqlSin _       _        = fail "Type Error!"                           
