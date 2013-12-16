{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Environment.Existential where

import qualified Environment.GADT as T

-- Environment Wrapper (existentials)
data Env where
  Env :: T.Env e -> Env
