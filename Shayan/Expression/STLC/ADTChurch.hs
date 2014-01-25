{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Expression.STLC.ADTChurch where

import Variable.ADT
import Data.Foldable(Foldable(..))
import Data.Traversable(Traversable(..))

-- ADT representation (Debruijn indices) of the simply-typed lambda calculus 
-- expressions with Integer constants and a built-in addition operator
data Exp t =
    Con Integer
  | Var Var
  | Abs t       (Exp t) 
  | App (Exp t) (Exp t) 
  | Add (Exp t) (Exp t) 
  deriving (Eq , Functor, Foldable, Traversable)
  -- Equality on expressions is always up to alpha equivalence 
  -- (thanks to Debruijn indices)    

instance Show a => Show (Exp a) where 
  show (Con i)               = show i 
  show (Var v)               = show v 
  show (Abs t eb)            = "(\\" ++ show t ++ " -> " ++ show eb ++ ")"
  show (App ef@(App _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef@(Add _ _) ea) = "(" ++ show ef ++ ") " ++ show ea
  show (App ef ea)           = show ef ++ " " ++ show ea
  show (Add el@(App _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el@(Add _ _) er) = "(" ++ show el ++ ") + " ++ show er 
  show (Add el er)           = show el ++ " + " ++ show er