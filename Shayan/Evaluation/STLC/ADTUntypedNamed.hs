{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module Evaluation.STLC.ADTUntypedNamed where

import Evaluation 
import Expression.STLC.ADTUntypedNamed
import qualified Value.STLC.ADT as V
import Environment.ADTTable as E
 
instance Eq v => Evl (Exp v) where
  type Val (Exp v)  = V.Val
  type Env (Exp v)  = E.Env v V.Val 
  evl (Con i)     _ = V.con i
  evl (Var x)     r = get x r
  evl (Abs x eb)  r = V.abs (\ va -> evl eb ((x , va) : r))             
  evl (App ef ea) r = do vf <- evl ef r
                         va <- evl ea r
                         V.app vf va 
  evl (Add el er) r = do vl <- evl el r 
                         vr <- evl er r      
                         V.add vl vr
