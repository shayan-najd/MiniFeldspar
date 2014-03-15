module Conversion.Expression.Feldspar.Evaluation.GADTTyped () where

import Prelude ()
import MyPrelude

import Expression.Feldspar.GADTTyped
import qualified Expression.Feldspar.ADTValue as V

import Environment.Scoped          

import Nat.ADT                     

import Conversion 
import Conversion.Variable ()

import Prelude (undefined)

instance Cnv (Exp n t , Env n V.Val) V.Val where
  cnv (ee , r) = let ?r = r in join (case ee of        
    ConI i       -> V.conI <$@> i
    ConB b       -> V.conB <$@> b 
    Var x        -> V.var  <$@> x
    Abs eb       -> V.abs  <$@> eb
    App _  ef ea -> V.app  <$@> ef <*@> ea 
    Cnd ec et ef -> V.cnd  <$@> ec <*@> et <*@> ef      
    Whl ec eb ei -> V.whl  <$@> ec <*@> eb <*@> ei
    Tpl ef es    -> V.tpl  <$@> ef <*@> es 
    Fst _  e     -> V.fst  <$@> e
    Snd _  e     -> V.snd  <$@> e 
    Ary el ef    -> V.ary  <$@> el <*@> ef
    Len _  e     -> V.len  <$@> e                         
    Ind ea ei    -> V.ind  <$@> ea <*@> ei                            
    Let _  el eb -> return <$@> App undefined (Abs eb) el)
    
instance Cnv (Exp (Suc n) t , Env n V.Val) (V.Val -> V.Val) where
  cnv (e , r) = pure (frmRgt . curry cnv e . (flip Ext r))