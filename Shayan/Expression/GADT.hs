{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Expression.GADT where

import Variable.GADT
import Type.GADT

-- GADT representation (Debruijn indices) of the simply-typed lambda calculus 
-- expressions with Integer constants and a built-in addition operator
data Exp e t where
  Con :: Int -> Exp e Int
  Add :: Exp e Int -> Exp e Int -> Exp e Int
  Var :: Var e t -> Exp e t
  Abs :: Typ ta -> Exp (ta , e) tb -> Exp e (ta -> tb)
  App :: Exp e (ta -> tb) -> Exp e ta -> Exp e tb
