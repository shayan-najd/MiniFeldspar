module Variable.Typed (Var(..) , prd , inc) where

import Prelude ()
import MyPrelude

data Var :: [k] -> k -> * where
  Zro :: Var (t ': r) t
  Suc :: Var r tp -> Var (t ': r) tp

deriving instance Eq   (Var e t)
deriving instance Ord  (Var e t)
deriving instance Show (Var e t)

prd :: Var (t' ': r) t -> Var r t
prd (Suc x) = x
prd _       = impossible
  
inc :: (forall t'. Var r t' -> Var r' t') -> Var (ta ': r) t -> Var (ta ': r') t
inc _ Zro     = Zro
inc f (Suc x) = Suc (f x)
 