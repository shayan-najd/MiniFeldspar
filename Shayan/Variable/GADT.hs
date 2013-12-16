{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Variable.GADT where

-- Variables
data Var e t where
  Zro :: Var (t , e) t
  Suc :: Var e tp -> Var (ts , e) tp
