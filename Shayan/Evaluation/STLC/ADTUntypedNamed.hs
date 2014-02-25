{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module Evaluation.STLC.ADTUntypedNamed where

import Evaluation 
import Expression.STLC.ADTUntypedNamed
import qualified Expression.STLC.ADTValue as V
import qualified Environment.ADTTable as E
 
type instance Val (Exp v)  = V.Val
type instance Env (Exp v)  = E.Env v V.Val 

instance Eq v => Evl (Exp v) where
  evl (Con i)     _ = V.con i
  evl (Var x)     r = E.get x r
  evl (Abs x eb)  r = V.abs (\ va -> evl eb ((x , va) : r))             
  evl (App ef ea) r = do vf <- evl ef r
                         va <- evl ea r
                         V.app vf va 
  evl (Add el er) r = do vl <- evl el r 
                         vr <- evl er r      
                         V.add vl vr
