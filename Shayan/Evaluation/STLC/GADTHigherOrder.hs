{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies  #-}
module Evaluation.STLC.GADTHigherOrder where

import Evaluation 
import Expression.STLC.GADTHigherOrder
import qualified Value.STLC.GADT as V
import Environment.GADT as E
import ErrorMonad
import Type.STLC.GADT

instance Evl (Exp r t) where
  type Val (Exp r t) = t
  type Env (Exp r t) = r 
  evl (Con i)     _ = V.con i
  evl (Var x)     r = return (get x r)
  evl (Abs t eb)  r = V.abs (flip evl r . eb . cnvValExp t r)
  evl (App ef ea) r = do vf <- evl ef r 
                         va <- evl ea r
                         V.app vf va
  evl (Add el er) r = do vl <- evl el r  
                         vr <- evl er r
                         V.add vl vr

cnvValExp :: Typ t -> r -> t -> Exp r t
cnvValExp Int         _ i = Con i
cnvValExp (Arr ta tb) r f = Abs ta (\ ea -> case evl ea r of    
                                       Rgt va -> cnvValExp tb r (f va) 
                                       Lft s  -> error s) 