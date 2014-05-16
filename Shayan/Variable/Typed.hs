module Variable.Typed where

import Prelude ()
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