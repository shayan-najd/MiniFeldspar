{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Expression.STLC.ADTChurch where

import Variable.ADT
import Data.Foldable(Foldable(..))
import Data.Traversable(Traversable(..))

data Exp t =
    Con Integer
  | Var Var
  | Abs t       (Exp t) 
  | App (Exp t) (Exp t) 
  | Add (Exp t) (Exp t) 
  deriving (Eq , Functor, Foldable, Traversable)
 