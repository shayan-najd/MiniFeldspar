{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module Evaluation.Feldspar.GADTFirstOrder where

import Evaluation 
import Expression.Feldspar.GADTFirstOrder
import Environment.GADT as E
import qualified Value.Feldspar.GADT as V
 
instance Evl (Exp e a) where
  type Val (Exp e a) = a
  type Env (Exp e a) = e
  evl (ConI i)    _  = V.conI i
  evl (ConB b)    _  = V.conB b
  evl (Var x)     r  = return (get x r)
  evl (Abs _ eb)  r  = V.abs (\ va -> evl eb (va , r))
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
  
  evl (Let t el eb)  r = do vl <- evl el r                          
                            vf <- evl (Abs t eb) r
                            V.app vf vl 