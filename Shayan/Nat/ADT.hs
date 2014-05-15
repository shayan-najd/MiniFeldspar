module Nat.ADT where

import Prelude (error)
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
prd _       = error "Bad use of Prd!"

inc :: (Nat -> Nat) -> Nat -> Nat
inc _ Zro     = Zro
inc f (Suc x) = Suc (f x)

infixl 6 +++
(+++) :: Nat -> Nat -> Nat
Zro     +++ n' = n'
(Suc n) +++ n' = Suc (n +++ n')

type N0 = Zro
type N1 = Suc N0
type N2 = Suc N1
type N3 = Suc N2 
type N4 = Suc N3 
type N5 = Suc N4 
type N6 = Suc N5 
type N7 = Suc N6 
type N8 = Suc N7 
type N9 = Suc N8 

pattern N0 = Zro
pattern N1 = Suc N0
pattern N2 = Suc N1
pattern N3 = Suc N2 
pattern N4 = Suc N3 
pattern N5 = Suc N4 
pattern N6 = Suc N5 
pattern N7 = Suc N6 
pattern N8 = Suc N7 
pattern N9 = Suc N8 
