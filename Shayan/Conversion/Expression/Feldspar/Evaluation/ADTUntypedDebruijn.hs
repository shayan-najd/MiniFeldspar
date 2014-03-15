module Conversion.Expression.Feldspar.Evaluation.ADTUntypedDebruijn () where

import Prelude   ()
import MyPrelude

import Expression.Feldspar.ADTUntypedDebruijn
import qualified Expression.Feldspar.ADTValue as V

import Environment.Plain 

import Conversion
import Conversion.Variable ()

instance Cnv (Exp , Env V.Val) V.Val where 
  cnv (ee , r) = let ?r = r in join (case ee of        
    ConI i       -> V.conI <$@> i
    ConB b       -> V.conB <$@> b 
    Var x        -> V.var  <$@> x
    Abs eb       -> V.abs  <$@> eb
    App ef ea    -> V.app  <$@> ef <*@> ea 
    Cnd ec et ef -> V.cnd  <$@> ec <*@> et <*@> ef      
    Whl ec eb ei -> V.whl  <$@> ec <*@> eb <*@> ei
    Tpl ef es    -> V.tpl  <$@> ef <*@> es 
    Fst e        -> V.fst  <$@> e
    Snd e        -> V.snd  <$@> e 
    Ary el ef    -> V.ary  <$@> el <*@> ef 
    Len e        -> V.len  <$@> e                         
    Ind ea ei    -> V.ind  <$@> ea <*@> ei                         
    Let el eb    -> return <$@> App (Abs eb) el)

instance Cnv (Exp , Env V.Val)  (V.Val -> V.Val) where
  cnv (e , r) = pure (frmRgt . curry cnv e . (: r))