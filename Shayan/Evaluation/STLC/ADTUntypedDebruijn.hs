{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module Evaluation.STLC.ADTUntypedDebruijn where

import Evaluation 
import Expression.STLC.ADTUntypedDebruijn
import qualified Expression.STLC.ADTValue as V
import qualified Environment.ADT as E
 
type instance Val Exp = V.Val
type instance Env Exp = E.Env V.Val 

instance Evl Exp where
  evl (Con i)     _ = V.con i
  evl (Var x)     r = E.get x r
  evl (Abs eb)    r = V.abs (\ va -> evl eb (va : r))
  evl (App ef ea) r = do vf <- evl ef r
                         va <- evl ea r
                         V.app vf va 
  evl (Add el er) r = do vl <- evl el r 
                         vr <- evl er r      
                         V.add vl vr