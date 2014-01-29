{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, ImplicitParams, ScopedTypeVariables #-}
module Evaluation.Feldspar.GADTFirstOrder where

import Evaluation 
import Evaluation.Variable.GADT ()
import Expression.Feldspar.GADTFirstOrder
import qualified Value.Feldspar.GADT as V
import Control.Applicative.Recursion
import ErrorMonad
 
instance Evl (Exp r t) where
  type Val (Exp r t) = t
  type Env (Exp r t) = r
  evl egfo r = case egfo of 
       ConI i       -> V.conI <$> pure i
       ConB b       -> V.conB <$> pure b
       Var x        -> V.var  <$> evl' x
       Abs _ eb     -> V.abs  <$> pure (\ va -> evl eb (va , r))
       App ef ea    -> V.app  <$> evl' ef <*> evl' ea
       Cnd ec et ef -> V.cnd  <$> evl' ec <*> evl' et <*> evl' ef
       Tpl ef es    -> V.tpl  <$> evl' ef <*> evl' es
       Fst e        -> V.fst  <$> evl' e
       Snd e        -> V.snd  <$> evl' e                       
       Ary el ef    -> V.arr  <$> evl' el <*> evl' ef
       Len e        -> V.len  <$> evl' e                         
       Ind ea ei    -> V.ind  <$> evl' ea <*> evl' ei                         
       Whl ec eb ei -> V.whl  <$> evl' ec <*> evl' eb <*> evl' ei
       Let t el eb  -> evl' (App (Abs t eb) el) 
       where
         evl' :: (Evl e , Env e ~ r) => e -> ErrM (Val e)
         evl' e  = evl e r 
