{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Expression.STLC.ADTExplicit where

import Variable.ADT
import Data.Foldable(Foldable(..))
import Data.Traversable(Traversable(..))

-- ADT representation (Debruijn indices) of the simply-typed lambda calculus 
-- (a la Curry) expressions with Integer constants and a built-in addition 
-- operator
data Exp t =
    Con t Integer
  | Var t Var
  | Abs t (Exp t) 
  | App t (Exp t) (Exp t)
  | Add t (Exp t) (Exp t) 
  deriving (Eq , Functor, Foldable, Traversable)
  -- Equality on expressions is always up to alpha equivalence 
  -- (thanks to Debruijn indices)   

instance Show t => Show (Exp t) where 
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