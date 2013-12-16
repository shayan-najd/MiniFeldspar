{-# OPTIONS_GHC -Wall #-}
module Expression.ADTUntyped where

import Variable.ADT

-- ADT representation (Debruijn indices) of the lambda calculus 
-- expressions with Integer constants and a built-in addition operator
data Exp =
    Con Int
  | Var Var
  | Abs Exp 
  | App Exp Exp 
  | Add Exp Exp 
  deriving Eq
  -- Equality on expressions is always up to alpha equivalence 
  -- (thanks to Debruijn indices)    

instance Show Exp where 
  show (Con i)               = show i 
  show (Var v)               = show v 
  show (Abs eb)              = "(\\ " ++ show eb ++ ")"
  show (App ef@(App _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef@(Add _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef ea)           = show ef ++ " " ++ show ea
  show (Add el@(App _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el@(Add _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el er)           = show el ++ " + " ++ show er 
