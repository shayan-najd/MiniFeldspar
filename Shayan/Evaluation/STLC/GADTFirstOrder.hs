{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module Evaluation.STLC.GADTFirstOrder where

import Evaluation as E
import Evaluation.Variable.GADT ()
import Expression.STLC.GADTFirstOrder
import qualified Value.STLC.GADT as V
import Control.Applicative.Recursion 
import ErrorMonad  

instance Evl (Exp r t) where
  type Val (Exp r t) = t
  type Env (Exp r t) = r 
  evl egfo r = case egfo of
    Con i     -> V.con <$> pure i
    Var x     -> V.var <$> evl' x
    Abs _ eb  -> V.abs <$> pure (\ va -> evl eb (va , r))
    App ef ea -> V.app <$> evl' ef <*> evl' ea
    Add el er -> V.add <$> evl' el <*> evl' er
    where
      evl' :: (Evl e, E.Env e ~ r) => e -> ErrM (Val e)
      evl' e = evl e r
