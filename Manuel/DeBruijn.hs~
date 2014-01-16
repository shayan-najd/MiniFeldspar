{-# LANGUAGE GADTs #-}

module DeBruijn where

import Text.Show.Functions


-- Type environments are nested pairs (..((), t1), t2, ..., tn)

-- Index projecting a specific type from a type environment; it's value
-- corresponds to a natural number, the de Brujin index
--
data Ix env t where
  ZeroIx ::             Ix (env, t) t
  SuccIx :: Ix env t -> Ix (env, s) t

instance Show (Ix env t) where
  show = show . ixToInt
    where
      ixToInt :: Ix env t -> Int
      ixToInt ZeroIx     = 0
      ixToInt (SuccIx n) = ixToInt n + 1

-- Lambda terms using de Bruijn indices to represent variables
--
data Term env t where
  Var :: Ix env t                        -> Term env t
  Con :: Show t
      => t                               -> Term env t
  Lam :: Term (env, s) t                 -> Term env (s -> t)
  App :: Term env (s -> t) -> Term env s -> Term env t

instance Show (Term env t) where
  show (Var ix)      = "{" ++ show ix ++ "}"
  show (Con c)       = show c
  show (Lam body)    = "(\\" ++ show body ++ ")"
  show (App fun arg) = "(" ++ show fun ++ " " ++ show arg ++ ")"

-- Valuation for a type environment
--
data Val env where
  Empty :: Val ()
  Push  :: Val env -> t -> Val (env, t)

-- Projection of a value from a valuation using a de Bruijn index
--
prj :: Ix env t -> Val env -> t
prj ZeroIx      (Push val v) = v
prj (SuccIx ix) (Push val _) = prj ix val

-- A term interpreter, evaluating a term under a valuation
--
intp :: Term env t -> Val env -> t
intp (Var ix)      val = prj ix val
intp (Con v)       val = v
intp (Lam body)    val = intp body . (val `Push`)
intp (App fun arg) val = (intp fun val) (intp arg val)
