module Nat.GADT
  (Nat(..),prd) where

import MyPrelude

import Singleton
import qualified Nat.ADT as NA

data Nat :: NA.Nat -> * where
  Zro :: Nat NA.Zro
  Suc :: Nat n -> Nat (NA.Suc n)

deriving instance Eq   (Nat n)
deriving instance Ord  (Nat n)

int :: Nat n -> Integer
int Zro     = 0
int (Suc x) = 1 + int x

instance Show (Nat n) where
  show v = show (int v)

prd :: Nat (NA.Suc n) -> Nat n
prd (Suc n) = n

type instance Trm (Nat NA.Zro)     = Maybe ()
type instance Trm (Nat (NA.Suc n)) = Maybe (Trm (Nat n))

instance HasSin Nat NA.Zro where
  sin = Zro

instance (HasSin Nat n) => HasSin Nat (NA.Suc n) where
  sin = Suc sin

instance EqlSin Nat where
  eqlSin Zro     Zro      = return Rfl
  eqlSin (Suc n) (Suc n') = do Rfl <- eqlSin n n'
                               return Rfl
  eqlSin _       _        = fail "Type Error!"

instance GetPrfHasSin Nat where
  getPrfHasSin Zro     = PrfHasSin
  getPrfHasSin (Suc n) = case getPrfHasSin n of
    PrfHasSin -> PrfHasSin