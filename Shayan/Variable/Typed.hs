module Variable.Typed
  (Var(..),prd,inc
  ,pattern N0,pattern N1,pattern N2,pattern N3,pattern N4,pattern N5
  ,pattern N6,pattern N7,pattern N8,pattern N9) where

import MyPrelude

data Var :: [k] -> k -> * where
  Zro :: Var (t ': r) t
  Suc :: Var r tp -> Var (t ': r) tp

deriving instance Eq   (Var e t)
deriving instance Ord  (Var e t)

int :: Var r t -> Integer
int Zro     = 0
int (Suc x) = 1 + int x

instance Show (Var r t) where
  show v = show (int v)
 
prd :: Var (t' ': r) t -> Var r t
prd (Suc x) = x
prd _       = impossible
  
inc :: (forall t'. Var r t' -> Var r' t') -> 
       Var (ta ': r) t -> Var (ta ': r') t
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