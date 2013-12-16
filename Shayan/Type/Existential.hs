{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Type.Existential where

import qualified Type.GADT as T

-- Typ Wrapper (existentials)
data Typ where
  Typ :: T.Typ t -> Typ
