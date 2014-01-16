{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Data.Nat.GADT where
 
data Zro  
data Suc n

data Nat n where
  Zro :: Nat Zro
  Suc :: Nat n -> Nat (Suc n)
  