{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Variable.Existential where

import qualified Variable.GADT as T
import qualified Type.GADT as T
import qualified Environment.GADT as T

-- Variable Wrapper (existentials)
data Var where
  Var :: T.Var e t -> T.Env e -> T.Typ t -> Var