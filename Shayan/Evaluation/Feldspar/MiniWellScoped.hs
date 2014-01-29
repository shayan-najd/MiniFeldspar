{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies,FlexibleInstances, ScopedTypeVariables #-}
module Evaluation.Feldspar.MiniWellScoped where

import Evaluation 
import Evaluation.Variable.GADT ()
import Expression.Feldspar.MiniWellScoped
import qualified Value.Feldspar.GADT as V
import ErrorMonad 
import Control.Applicative.Recursion

instance Evl (Exp r t) where
  type Val (Exp r t) = t
  type Env (Exp r t) = r
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
       Prm1 vf e       -> evl' vf <*> evl' e
       Prm2 vf e1 e2   -> evl' vf <*> evl' e1 <*> evl' e2
       Val x           -> return x
       Undef           -> undefined                                
       Var x           -> V.var  <$> evl' x                             
       where
         evl' :: (Evl e, Env e ~ r) => e -> ErrM (Val e)
         evl' e  = evl e r 
 
instance Evl (Exp r ta -> Exp r tb) where
  type Val (Exp r ta -> Exp r tb) = ta -> tb
  type Env (Exp r ta -> Exp r tb) = r
  evl f r = return (frmRgt . flip evl r . f . Val) 