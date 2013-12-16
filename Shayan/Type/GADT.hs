{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Type.GADT where

-- Types (Singleton)
data Typ t where
  Int :: Typ Int
  Arr :: Typ ta -> Typ tb -> Typ (ta -> tb)


