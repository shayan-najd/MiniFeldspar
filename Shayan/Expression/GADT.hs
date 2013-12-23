{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
module Expression.GADT where

import Variable.GADT
import Type.GADT

-- GADT representation (Debruijn indices) of the simply-typed lambda calculus 
-- expressions with Integer constants and a built-in addition operator
data Exp e t where
  Con :: Integer -> Exp e Integer
  Add :: Exp e Integer -> Exp e Integer -> Exp e Integer
  Var :: Var e t -> Exp e t
  Abs :: Typ ta -> Exp (ta , e) tb -> Exp e (ta -> tb)
  App :: Exp e (ta -> tb) -> Exp e ta -> Exp e tb

instance Show (Exp e t) where 
  show (Con i)               = show i 
  show (Var v)               = show v 
  show (Abs _ eb)            = "(\\ " ++ show eb ++ ")"
  show (App ef@(App _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef ea)           = show ef ++ " " ++ show ea
  show (Add el@(App _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el@(Add _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el er)           = show el ++ " + " ++ show er 
