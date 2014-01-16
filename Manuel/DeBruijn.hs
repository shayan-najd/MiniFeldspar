{-# LANGUAGE GADTs #-}

module DeBruijn where

import Text.Show.Functions


-- Type environments are nested pairs (..((), t1), t2, ..., tn)

-- Index projecting a specific type from a type environment; it's value
-- corresponds to a natural number, the de Brujin index
--
data Var r t where
  Zro :: Var (r, t) t
  Suc :: Var r t -> Var (r, t') t

instance Show (Var r t) where
  show = show . ixToInt
    where
      ixToInt :: Var r t -> Int
      ixToInt Zro     = 0
      ixToInt (Suc n) = ixToInt n + 1

-- Lambda terms using de Bruijn indices to represent variables
--
data Exp r t where
  Var :: Var r t -> Exp r t
  Con :: Show t => 
         t -> Exp r t
  Lam :: Exp (r , ta) tb -> Exp r (ta -> tb)
  App :: Exp r (ta -> tb) -> Exp r ta -> Exp r tb

instance Show (Exp r t) where
  show (Var ix)      = "{" ++ show ix ++ "}"
  show (Con c)       = show c
  show (Lam body)    = "(\\" ++ show body ++ ")"
  show (App fun arg) = "(" ++ show fun ++ " " ++ show arg ++ ")"

-- Valuation for a type environment
--
data Env r where
  Emp :: Env ()
  Ext :: Env r -> t -> Env (r, t)

-- Projection of a value from a valuation using a de Bruijn index
--
get :: Var r t -> Env r -> t
get Zro      (Ext val v) = v
get (Suc ix) (Ext val _) = get ix val

-- A term interpreter, evaluating a term under a valuation
--
evl :: Exp r t -> Env r -> t
evl (Con v)     _ = v
evl (Var x)     r = get x r
evl (Lam eb)    r = evl eb . (r `Ext`)
evl (App ef ea) r = evl ef r $ evl ea r
