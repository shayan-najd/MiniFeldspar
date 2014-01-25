{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module Evaluation.STLC.GADTFirstOrder where

import Evaluation 
import Expression.STLC.GADTFirstOrder
import qualified Value.STLC.GADT as V
import Environment.GADT as E
 
instance Evl (Exp e a) where
  type Val (Exp e a) = a
  type Env (Exp e a) = e 
  evl (Con i)     _ = V.con i
  evl (Var x)     r = return (get x r)
  evl (Abs _ eb)  r = V.abs (\ va -> evl eb (va , r))
  evl (App ef ea) r = do vf <- evl ef r 
                         va <- evl ea r
                         V.app vf va
  evl (Add el er) r = do vl <- evl el r  
                         vr <- evl er r
                         V.add vl vr
