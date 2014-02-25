{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, ImplicitParams, ScopedTypeVariables #-}
module Evaluation.Feldspar.GADTFirstOrder where

import Evaluation 
import Evaluation.Variable.GADT ()
import Expression.Feldspar.GADTFirstOrder
import qualified Value.Feldspar.GADT as V
import Control.Applicative.Recursion
import Singleton 
import qualified Singleton.Environment as E
import Evaluation.Variable.GADT ()
  
type instance Val (Exp r t) = Trm t
type instance Env (Exp r t) = Trm r

instance Evl (Exp r t) where
  evl egfo r = case egfo of 
       ConI i       -> V.conI <$> pure i
       ConB b       -> V.conB <$> pure b
       Var x        -> return (E.get x r)
       Abs _ eb     -> V.abs  <$> pure (\ va -> evl eb (va , r))
       App ef ea    -> V.app  <$> evl ef r <*> evl ea r
       Cnd ec et ef -> V.cnd  <$> evl ec r <*> evl et r <*> evl ef r
       Tpl ef es    -> V.tpl  <$> evl ef r <*> evl es r
       Fst e        -> V.fst  <$> evl e  r
       Snd e        -> V.snd  <$> evl e  r                      
       Ary el ef    -> V.arr  <$> evl el r <*> evl ef r
       Len e        -> V.len  <$> evl e  r                       
       Ind ea ei    -> V.ind  <$> evl ea r <*> evl ei r                       
       Whl ec eb ei -> V.whl  <$> evl ec r <*> evl eb r <*> evl ei r
       Let t el eb  -> evl (App (Abs t eb) el) r 
        