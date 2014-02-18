{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies,ScopedTypeVariables #-}
module Evaluation.Feldspar.GADTHigherOrder where

import Evaluation 
import Evaluation.Variable.GADT ()
import Expression.Feldspar.GADTHigherOrder
import qualified Value.Feldspar.GADT as V
import Control.Applicative.Recursion
import ErrorMonad
import Data.Array
import Data.Maybe(fromJust)
import Singleton 
import qualified Singleton.TypeFeldspar as G  
import Singleton.TypeFeldspar ()
import Singleton.Environment(get)

instance Evl (Exp r t) where
  type Val (Exp r t) = Trm t
  type Env (Exp r t) = Trm r
  evl egfo r = case egfo of 
       ConI i       -> V.conI <$> pure i
       ConB b       -> V.conB <$> pure b
       Var x        -> return (get x r)
       App ef ea    -> V.app  <$> evl ef r <*> evl ea r
       Abs t eb     -> V.abs  <$> pure (flip evl r . eb . cnvValExp t r)
       Cnd ec et ef -> V.cnd  <$> evl ec r <*> evl et r <*> evl ef r
       Tpl ef es    -> V.tpl  <$> evl ef r <*> evl es r
       Fst e        -> V.fst  <$> evl e  r
       Snd e        -> V.snd  <$> evl e  r                      
       Ary el ef    -> V.arr  <$> evl el r <*> evl ef r
       Len e        -> V.len  <$> evl e  r                        
       Ind ea ei    -> V.ind  <$> evl ea r <*> evl ei r                        
       Whl ec eb ei -> V.whl  <$> evl ec r <*> evl eb r <*> evl ei r
       Let t el eb  -> evl (App (Abs t eb) el) r
 
 
cnvValExp :: G.Typ t -> Trm r -> Trm t -> Exp r t
cnvValExp G.Int         _ i         = ConI i
cnvValExp G.Bol         _ b         = ConB b  
cnvValExp (G.Arr ta tb) r f         = Abs ta (cnvValExp tb r . f . frmRgt 
                                              . flip evl r)
cnvValExp (G.Tpl tf ts) r (vf , vs) = Tpl (cnvValExp tf r vf) (cnvValExp ts r vs)
cnvValExp (G.Ary ta)    r va        = let (0,l) = bounds va in
  Ary (ConI l) (cnvValExp (G.Arr G.Int ta) r 
                (fromJust . flip lookup (assocs va)))
  