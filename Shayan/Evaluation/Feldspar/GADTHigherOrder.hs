{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module Evaluation.Feldspar.GADTHigherOrder where

import Evaluation 
import Expression.Feldspar.GADTHigherOrder
import Environment.GADT as E
import qualified Value.Feldspar.GADT as V
import qualified Type.Feldspar.GADT  as G
import ErrorMonad
import Data.Array
import Data.Maybe(fromJust)
 
instance Evl (Exp e a) where
  type Val (Exp e a) = a
  type Env (Exp e a) = e
  evl (ConI i)    _  = V.conI i
  evl (ConB b)    _  = V.conB b
  evl (Var x)     r  = return (get x r)
  evl (Abs t eb)  r  = V.abs (flip evl r . eb . cnvValExp t r)
  evl (App ef ea) r  = do vf <- evl ef r 
                          va <- evl ea r      
                          V.app vf va
 
  evl (Cnd ec et ef) r = do vc <- evl ec r 
                            vt <- evl et r      
                            vf <- evl ef r      
                            V.cnd vc vt vf
 
  evl (Tpl ef es) r = do vf <- evl ef r 
                         vs <- evl es r      
                         V.tpl vf vs
  
  evl (Fst e)     r = do v <- evl e r                         
                         V.fst v
  
  evl (Snd e)     r = do v <- evl e r                         
                         V.snd v                       
  
  evl (Ary el ef) r = do vl <- evl el r
                         vf <- evl ef r
                         V.arr vl vf 

  evl (Len e)     r = do e' <- evl e r
                         V.len e'
                         
  evl (Ind ea ei) r = do va <- evl ea r                       
                         vi <- evl ei r
                         V.ind va vi
                         
  evl (Whl ec eb ei) r = do vc <- evl ec r
                            vb <- evl eb r
                            vi <- evl ei r
                            V.whl vc vb vi
  
  evl (Let tl el eb)  r = do vl <- evl el r                          
                             vf <- evl (Abs tl eb) r
                             V.app vf vl 
                                                    
cnvValExp :: G.Typ t -> r -> t -> Exp r t
cnvValExp G.Int         _ i = ConI i
cnvValExp (G.Arr ta tb) r f = Abs ta (\ ea -> case evl ea r of    
                                         Rgt va -> cnvValExp tb r (f va) 
                                         Lft s  -> error s)       
cnvValExp G.Bol         _ b = ConB b                            
cnvValExp (G.Tpl tf ts) r (vf , vs) = Tpl (cnvValExp tf r vf) (cnvValExp ts r vs)
cnvValExp (G.Ary ta)    r va        = let (0,l) = bounds va in
  Ary (ConI l) 
  (cnvValExp (G.Arr G.Int ta) r (\ i -> fromJust (lookup i (assocs va))))
  