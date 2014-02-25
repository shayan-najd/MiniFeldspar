{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies,FlexibleInstances #-}
module Evaluation.Feldspar.Mini where

import Evaluation 
import Expression.Feldspar.Mini
import qualified Value.Feldspar.GADT as V
import ErrorMonad 
import Control.Applicative.Recursion

type instance Val (Exp t) = t
type instance Env (Exp t) = ()

instance Evl (Exp t) where
  evl egfo r = case egfo of 
       ConI i          -> V.conI <$> pure i
       ConB b          -> V.conB <$> pure b
       Cnd ec et ef    -> V.cnd  <$> evl' ec <*> evl' et <*> evl' ef        
       Tpl ef es       -> V.tpl  <$> evl' ef <*> evl' es
       Fst e           -> V.fst  <$> evl' e
       Snd e           -> V.snd  <$> evl' e                       
       Ary el ef       -> V.arr  <$> evl' el <*> evl' ef
       Len e           -> V.len  <$> evl' e                         
       Ind ea ei       -> V.ind  <$> evl' ea <*> evl' ei                         
       Whl ec eb ei    -> V.whl  <$> evl' ec <*> evl' eb <*> evl' ei
       Let el eb       -> (evl' . eb) =<< evl' el                         
       Prm1 _ vf e     -> vf     <$> evl' e
       Prm2 _ vf e1 e2 -> vf     <$> evl' e1 <*> evl' e2
       Val x           -> return x
       Undef           -> undefined                                
       Var _           -> undefined                             
       where
         evl' :: (Evl t, Env t ~ ()) => t-> ErrM (Val t)
         evl' e  = evl e r 
 
type instance Val (Exp ta -> Exp tb) = ta -> tb
type instance Env (Exp ta -> Exp tb) = ()
instance Evl (Exp ta -> Exp tb) where

  evl f r = return (frmRgt . flip evl r . f . Val) 