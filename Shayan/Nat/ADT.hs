module Nat.ADT
  (Nat(..),prd,inc) where


import MyPrelude

data Nat =
    Zro
  | Suc Nat

deriving instance Eq   Nat
deriving instance Ord  Nat

int :: Nat -> Integer
int Zro     = 0
int (Suc x) = 1 + int x

instance Show Nat where
  show v = show (int v)

prd :: Nat -> Nat
prd (Suc n) = n
prd _       = badUse "Prd"

inc :: (Nat -> Nat) -> Nat -> Nat
inc _ Zro     = Zro
inc f (Suc x) = Suc (f x)