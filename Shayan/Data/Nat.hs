module Data.Nat where

data Nat =
    Zro
  | Suc Nat
  deriving (Eq , Ord)
           
prd :: Nat -> Nat           
prd (Suc n) = n
prd _       = error "Bad use of Prd!"

infixl 6 +++
(+++) :: Nat -> Nat -> Nat
Zro     +++ n' = n'
(Suc n) +++ n' = Suc (n +++ n')

