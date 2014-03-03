module Data.Fin where
 
import qualified Data.Nat as A

data Nat :: A.Nat -> * where
  Zro :: Nat (A.Suc n)
  Suc :: Nat n -> Nat (A.Suc n)  
                               
deriving instance Eq (Nat n)