module Conversion.Expression.Feldspar.Evaluation.ADTUntypedDebruijn () where

import MyPrelude

import Expression.Feldspar.ADTUntypedDebruijn
import qualified Expression.Feldspar.ADTValue as FAV

import Environment.Plain 

import Conversion
import Conversion.Variable ()

instance Cnv (Exp , Env FAV.Exp) FAV.Exp where 
  cnv (ee , r) = let ?r = r in join (case ee of        
    ConI i       -> FAV.conI <$@> i
    ConB b       -> FAV.conB <$@> b 
    ConF b       -> FAV.conF <$@> b 
    Var x        -> FAV.var  <$@> x
    Abs eb       -> FAV.abs  <$@> eb
    App ef ea    -> FAV.app  <$@> ef <*@> ea 
    Cnd ec et ef -> FAV.cnd  <$@> ec <*@> et <*@> ef      
    Whl ec eb ei -> FAV.whl  <$@> ec <*@> eb <*@> ei
    Tpl ef es    -> FAV.tpl  <$@> ef <*@> es 
    Fst e        -> FAV.fst  <$@> e
    Snd e        -> FAV.snd  <$@> e 
    Ary el ef    -> FAV.ary  <$@> el <*@> ef 
    Len e        -> FAV.len  <$@> e                         
    Ind ea ei    -> FAV.ind  <$@> ea <*@> ei                         
    Let el eb    -> pure     <$@> App (Abs eb) el
    Cmx er ei    -> FAV.cmx  <$@> er <*@> ei)

instance Cnv (Exp , Env FAV.Exp)  (FAV.Exp -> FAV.Exp) where
  cnv (e , r) = pure (frmRgt . curry cnv e . (: r))