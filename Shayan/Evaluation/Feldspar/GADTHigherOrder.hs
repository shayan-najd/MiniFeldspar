{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies,ScopedTypeVariables #-}
module Evaluation.Feldspar.GADTHigherOrder where

import Evaluation 
import Evaluation.Variable.GADT ()
import Expression.Feldspar.GADTHigherOrder
import qualified Value.Feldspar.GADT as V
import qualified Type.Feldspar.GADT  as G
import Control.Applicative.Recursion
import ErrorMonad
import Data.Array
import Data.Maybe(fromJust)
 
instance Evl (Exp r t) where
  type Val (Exp r t) = t
  type Env (Exp r t) = r
  evl egfo r = case egfo of 
       ConI i       -> V.conI <$> pure i
       ConB b       -> V.conB <$> pure b
       Var x        -> V.var  <$> evl' x
       App ef ea    -> V.app  <$> evl' ef <*> evl' ea
       Abs t eb     -> V.abs  <$> pure (evl' . eb . cnvValExp t r)
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
 
cnvValExp :: G.Typ t -> r -> t -> Exp r t
cnvValExp G.Int         _ i         = ConI i
cnvValExp G.Bol         _ b         = ConB b  
cnvValExp (G.Arr ta tb) r f         = Abs ta (cnvValExp tb r . f . frmRgt 
                                              . flip evl r)
cnvValExp (G.Tpl tf ts) r (vf , vs) = Tpl (cnvValExp tf r vf) (cnvValExp ts r vs)
cnvValExp (G.Ary ta)    r va        = let (0,l) = bounds va in
  Ary (ConI l) (cnvValExp (G.Arr G.Int ta) r 
                (fromJust . flip lookup (assocs va)))
  