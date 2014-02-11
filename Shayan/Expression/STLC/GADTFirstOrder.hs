{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
module Expression.STLC.GADTFirstOrder where

import Prelude hiding (sin)
import Variable.GADT
import Type.STLC.GADT
import Singleton

-- GADT representation (Debruijn indices) of the simply-typed lambda calculus 
-- expressions with Integer constants and a built-in addition operator
data Exp r t where
  Con :: Integer -> Exp r Integer
  Add :: Exp r Integer -> Exp r Integer -> Exp r Integer
  Var :: Var r t -> Exp r t
  Abs :: Typ ta -> Exp (ta , r) tb -> Exp r (ta -> tb)
  App :: Exp r (ta -> tb) -> Exp r ta -> Exp r tb
  
instance Show (Exp r t) where 
  show (Con i)               = show i 
  show (Var v)               = show v 
  show (Abs _ eb)            = "(\\ " ++ show eb ++ ")"
  show (App ef@(App _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef ea)           = show ef ++ " " ++ show ea
  show (Add el@(App _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el@(Add _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el er)           = show el ++ " + " ++ show er 

abs :: Sin Typ ta => Exp (ta , r) tb -> Exp r (ta -> tb) 
abs = Abs sin