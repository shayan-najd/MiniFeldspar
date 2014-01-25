{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module Evaluation.STLC.ADTUntypedDebruijn where

import Evaluation 
import Expression.STLC.ADTUntypedDebruijn
import qualified Value.STLC.ADT as V
import Environment.ADT as E
 
instance Evl Exp where
  type Val Exp = V.Val
  type Env Exp = E.Env V.Val 
  evl (Con i)     _ = V.con i
  evl (Var x)     r = get x r
  evl (Abs eb)    r = V.abs (\ va -> evl eb (va : r))
  evl (App ef ea) r = do vf <- evl ef r
                         va <- evl ea r
                         V.app vf va 
  evl (Add el er) r = do vl <- evl el r 
                         vr <- evl er r      
                         V.add vl vr