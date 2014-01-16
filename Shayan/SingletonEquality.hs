{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module SingletonEquality where

import ErrorMonad

-- Proof of Equality
data Eql a b where
  Rfl :: Eql a a 
  --Reflexivity as a proof of equality

-- Equality of singletons
class EqlSin tf where
  eqlSin :: tf t -> tf t' -> ErrM (Eql t t')
  
 