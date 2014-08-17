module Variable.Scoped
  (Var(..),prd,inc
  ,pattern N0,pattern N1,pattern N2,pattern N3,pattern N4,pattern N5
  ,pattern N6,pattern N7,pattern N8,pattern N9) where

import MyPrelude

import qualified Nat.ADT as NA

data Var :: NA.Nat -> * where
  Zro :: Var (NA.Suc n)
  Suc :: Var n -> Var (NA.Suc n)  
                               
deriving instance Eq   (Var n)
deriving instance Ord  (Var n)
deriving instance Show (Var n)

prd :: Var (NA.Suc n) -> Var n
prd (Suc x) = x
prd _       = impossible

inc :: (Var n -> Var n') ->
       Var (NA.Suc n) -> Var (NA.Suc n')
inc _ Zro     = Zro
inc f (Suc x) = Suc (f x)

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