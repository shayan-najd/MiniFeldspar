{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Expression.STLC.ADTExplicit where

import Variable.ADT
import Data.Foldable(Foldable(..))
import Data.Traversable(Traversable(..))
 
data Exp t =
    Con t Integer
  | Var t Var
  | Abs t (Exp t) 
  | App t (Exp t) (Exp t)
  | Add t (Exp t) (Exp t) 
  deriving (Eq , Functor, Foldable, Traversable)