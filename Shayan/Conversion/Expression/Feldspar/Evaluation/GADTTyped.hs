module Conversion.Expression.Feldspar.Evaluation.GADTTyped () where

import Prelude ()
import MyPrelude

import Expression.Feldspar.GADTTyped
import qualified Expression.Feldspar.ADTValue as FAV

import Environment.Scoped          

import Nat.ADT                     

import Conversion 
import Conversion.Variable ()

import Prelude (undefined)

instance Cnv (Exp n t , Env n FAV.Exp) FAV.Exp where
  cnv (ee , r) = let ?r = r in join (case ee of        
    ConI i       -> FAV.conI <$@> i
    ConB b       -> FAV.conB <$@> b 
    Var x        -> FAV.var  <$@> x
    Abs eb       -> FAV.abs  <$@> eb
    App _  ef ea -> FAV.app  <$@> ef <*@> ea 
    Cnd ec et ef -> FAV.cnd  <$@> ec <*@> et <*@> ef      
    Whl ec eb ei -> FAV.whl  <$@> ec <*@> eb <*@> ei
    Tpl ef es    -> FAV.tpl  <$@> ef <*@> es 
    Fst _  e     -> FAV.fst  <$@> e
    Snd _  e     -> FAV.snd  <$@> e 
    Ary el ef    -> FAV.ary  <$@> el <*@> ef
    Len _  e     -> FAV.len  <$@> e                         
    Ind ea ei    -> FAV.ind  <$@> ea <*@> ei                            
    Let _  el eb -> return <$@> App undefined (Abs eb) el)
    
instance Cnv (Exp (Suc n) t , Env n FAV.Exp) (FAV.Exp -> FAV.Exp) where
  cnv (e , r) = pure (frmRgt . curry cnv e . (flip Ext r))