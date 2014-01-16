{-# OPTIONS_GHC -Wall #-}
module Expression.STLC.ADTChurchMonomorphic where

import Variable.ADT
import Type.STLC.ADTSimple

-- ADT representation (Debruijn indices) of the simply-typed lambda calculus 
-- expressions with Integer constants and a built-in addition operator
data Exp =
    Con Integer
  | Var Var
  | Abs Typ Exp 
  | App Exp Exp 
  | Add Exp Exp 
  deriving Eq
  -- Equality on expressions is always up to alpha equivalence 
  -- (thanks to Debruijn indices)    

instance Show Exp where 
  show (Con i)               = show i 
  show (Var v)               = show v 
  show (Abs t eb)            = "(\\" ++ show t ++ " -> " ++ show eb ++ ")"
  show (App ef@(App _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef@(Add _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef ea)           = show ef ++ " " ++ show ea
  show (Add el@(App _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el@(Add _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el er)           = show el ++ " + " ++ show er 