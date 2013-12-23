{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Value.Existential where

import Type.GADT
 
-- Values
data Val where 
  Val :: a -> Typ a -> Val
  