{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module Evaluation.STLC.ADTExplicitPolymorphic where

import Evaluation 
import Expression.STLC.ADTExplicitPolymorphic
import qualified Value.STLC.ADT as V
import Environment.ADT as E
 
instance Evl (Exp a) where
  type Val (Exp a) = V.Val
  type Env (Exp a) = E.Env V.Val 
  evl (Con _ i)     _ = V.con i
  evl (Var _ x)     r = get x r
  evl (Abs _ eb)    r = V.abs (\ va -> evl eb (va : r))
  evl (App _ ef ea) r = do vf <- evl ef r
                           va <- evl ea r
                           V.app vf va 
  evl (Add _ el er) r = do vl <- evl el r 
                           vr <- evl er r      
                           V.add vl vr
