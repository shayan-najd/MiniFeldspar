{-# OPTIONS_GHC -Wall #-}
module Environment.ADT where

import Variable.ADT(Var(..),Nat)

type Env a = [a]

-- Extraction of values from environment
get :: Monad m => Nat -> [a] -> m a
get Zro     (x : _ ) = return x
get (Suc n) (_ : xs) = get n xs
get _       []       = fail "Scope Error!"
