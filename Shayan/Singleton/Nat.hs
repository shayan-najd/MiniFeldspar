{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleContexts #-}
module Singleton.Nat where
 
import Prelude ()
import Singleton

data Zro  
data Suc n

data Nat n where
  Zro :: Nat Zro
  Suc :: Nat n -> Nat (Suc n)  
  
instance HasSin Nat Zro where  
  sin = Zro
  
instance (HasSin Nat n) => HasSin Nat (Suc n) where  
  sin = Suc sin
  
  