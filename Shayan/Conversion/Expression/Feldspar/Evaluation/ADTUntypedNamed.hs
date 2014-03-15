module Conversion.Expression.Feldspar.Evaluation.ADTUntypedNamed () where

import Prelude   ()
import MyPrelude

import Expression.Feldspar.ADTUntypedNamed
import qualified Expression.Feldspar.ADTValue as V

import Environment.Map             

import Conversion  
import Conversion.Variable ()

instance Eq v => Cnv (Exp v , Env v V.Val) V.Val where
  cnv (ee , r) = let ?r = r in join (case ee of        
    ConI i             -> V.conI <$@> i
    ConB b             -> V.conB <$@> b 
    Var x              -> V.var  <$@> x
    Abs x eb           -> V.abs  <$@> (x , eb)
    App ef ea          -> V.app  <$@> ef <*@> ea 
    Cnd ec et ef       -> V.cnd  <$@> ec <*@> et <*@> ef      
    Whl xc ec xb eb ei -> V.whl  <$@> (xc , ec) <*@> (xb , eb) <*@> ei
    Tpl ef es          -> V.tpl  <$@> ef <*@> es 
    Fst e              -> V.fst  <$@> e
    Snd e              -> V.snd  <$@> e 
    Ary el x ef        -> V.ary  <$@> el <*@> (x , ef)
    Len e              -> V.len  <$@> e                         
    Ind ea ei          -> V.ind  <$@> ea <*@> ei                         
    Let x el eb        -> return <$@> App (Abs x eb) el)
        
instance Eq v => Cnv ((v , Exp v) , Env v V.Val) (V.Val -> V.Val) where
  cnv ((x , e) , r) = pure (frmRgt . curry cnv e . (: r) . (,) x) 