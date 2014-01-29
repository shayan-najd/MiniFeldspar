{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies,ScopedTypeVariables  #-}
module Evaluation.STLC.GADTHigherOrder where

import Evaluation 
import Evaluation.Variable.GADT ()
import Expression.STLC.GADTHigherOrder
import Type.STLC.GADT
import qualified Value.STLC.GADT as V
import Control.Applicative.Recursion
import ErrorMonad

instance Evl (Exp r t) where
  type Val (Exp r t) = t
  type Env (Exp r t) = r 
  evl egho r = case egho of
    Con i     -> V.con <$> pure i
    Var x     -> V.var <$> evl' x  
    Abs t  eb -> V.abs <$> pure (flip evl r . eb . cnvValExp t r)
    App ef ea -> V.app <$> evl' ef <*> evl' ea
    Add el er -> V.add <$> evl' el <*> evl' er
    where evl' :: (Evl e , Env e ~ r) => e -> ErrM (Val e)
          evl' e = evl e r 

cnvValExp :: Typ t -> r -> t -> Exp r t
cnvValExp Int         _ i = Con i
cnvValExp (Arr ta tb) r f = Abs ta (cnvValExp tb r . f . frmRgt . flip evl r)
                             