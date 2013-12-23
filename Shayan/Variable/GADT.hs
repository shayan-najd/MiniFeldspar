{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Variable.GADT where

-- Variables
data Var e t where
  Zro :: Var (t , e) t
  Suc :: Var e tp -> Var (ts , e) tp

instance Show (Var e t) where
 show = ("x"++) . show . int

int :: Var e t -> Int  
int Zro     =  0
int (Suc v) =  1 + int v 
 