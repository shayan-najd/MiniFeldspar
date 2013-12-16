{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Expression.Existential where

import qualified Expression.GADT as T
import qualified Type.GADT as T
import qualified Environment.GADT as T

-- Expression Wrapper (existentials)
data Exp where
  Exp :: T.Exp e t -> T.Env e -> T.Typ t -> Exp
