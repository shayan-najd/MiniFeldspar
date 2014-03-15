module Nat.ADT where

import Prelude (error)
import MyPrelude 

data Nat =
    Zro
  | Suc Nat
    
deriving instance Eq   Nat
deriving instance Ord  Nat
deriving instance Show Nat
           
prd :: Nat -> Nat           
prd (Suc n) = n
prd _       = error "Bad use of Prd!"

inc :: (Nat -> Nat) -> Nat -> Nat
inc _ Zro     = Zro
inc f (Suc x) = Suc (f x)

infixl 6 +++
(+++) :: Nat -> Nat -> Nat
Zro     +++ n' = n'
(Suc n) +++ n' = Suc (n +++ n')

