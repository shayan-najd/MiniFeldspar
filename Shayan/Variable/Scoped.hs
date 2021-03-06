module Variable.Scoped
  (Var(..),prd,inc) where

import MyPrelude

import qualified Nat.ADT as NA

data Var :: NA.Nat -> * where
  Zro :: Var (NA.Suc n)
  Suc :: Var n -> Var (NA.Suc n)

deriving instance Eq   (Var n)
deriving instance Ord  (Var n)

int :: Var n -> Integer
int Zro     = 0
int (Suc x) = 1 + int x

instance Show (Var n) where
  show v = show (int v)

prd :: Var (NA.Suc n) -> Var n
prd (Suc x) = x
prd _       = impossible

inc :: (Var n -> Var n') ->
       Var (NA.Suc n) -> Var (NA.Suc n')
inc _ Zro     = Zro
inc f (Suc x) = Suc (f x)