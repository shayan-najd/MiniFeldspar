{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
module Expression.ADTExplicitPolymorphic where

import Variable.ADT

-- ADT representation (Debruijn indices) of the simply-typed lambda calculus 
-- (a la Curry) expressions with Integer constants and a built-in addition 
-- operator
data Exp a =
    Con a Integer
  | Var a Var
  | Abs a (Exp a) 
  | App a (Exp a) (Exp a)
  | Add a (Exp a) (Exp a) 
  deriving (Eq , Functor)
  -- Equality on expressions is always up to alpha equivalence 
  -- (thanks to Debruijn indices)   

instance Show a => Show (Exp a) where 
  show (Con t i)                 = "(" ++ show i ++ " : " ++ show t ++ ")"
  show (Var t v)                 = "(" ++ show v ++ " : " ++ show t ++ ")"
  show (Abs t eb)                = "(\\ " ++ show eb ++ " : " ++ show t ++ ")"
  show (App t ef@(App _ _ _) ea) = "((" ++ show ef ++ ") " ++ show ea
                                   ++ " : " ++ show t ++ ")"
  show (App t ef@(Add _ _ _) ea) = "((" ++ show ef ++ ") " ++ show ea
                                   ++ " : " ++ show t ++ ")"
  show (App t ef ea)             = "(" ++ show ef ++ " " ++ show ea
                                   ++ " : " ++ show t ++ ")"
  show (Add t el@(App _ _ _) er) = "((" ++ show el ++ ") + " ++ show er 
                                   ++ " : " ++ show t ++ ")"
  show (Add t el@(Add _ _ _) er) = "((" ++ show el ++ ") + " ++ show er 
                                   ++ " : " ++ show t ++ ")"
  show (Add t el er)             = "(" ++ show el ++ " + " ++ show er 
                                   ++ " : " ++ show t ++ ")"