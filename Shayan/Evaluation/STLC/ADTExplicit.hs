{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module Evaluation.STLC.ADTExplicit where

import Evaluation 
import Expression.STLC.ADTExplicit
import qualified Expression.STLC.ADTValue as V
import qualified Environment.ADT as E
 
type instance Val (Exp a) = V.Val
type instance Env (Exp a) = E.Env V.Val 

instance Evl (Exp a) where
  evl (Con _ i)     _ = V.con i
  evl (Var _ x)     r = E.get x r
  evl (Abs _ eb)    r = V.abs (\ va -> evl eb (va : r))
  evl (App _ ef ea) r = do vf <- evl ef r
                           va <- evl ea r
                           V.app vf va 
  evl (Add _ el er) r = do vl <- evl el r 
                           vr <- evl er r      
                           V.add vl vr
