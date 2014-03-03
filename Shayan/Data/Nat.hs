module Data.Nat where

data Nat =
    Zro
  | Suc Nat
  deriving (Eq , Ord)


