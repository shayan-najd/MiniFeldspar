module Conversion.Expression.Feldspar.Evaluation.ADTUntypedNamed () where

import MyPrelude

import Expression.Feldspar.ADTUntypedNamed
import qualified Expression.Feldspar.ADTValue as FAV

import Environment.Map

import Conversion
import Conversion.Variable ()

instance Eq v => Cnv (Exp v , Env v FAV.Exp) FAV.Exp where
  cnv (ee , r) = let ?r = r in join (case ee of
    ConI i             -> FAV.conI <$@> i
    ConB b             -> FAV.conB <$@> b
    ConF b             -> FAV.conF <$@> b
    Var x              -> FAV.var  <$@> x
    Abs x eb           -> FAV.abs  <$@> (x , eb)
    App ef ea          -> FAV.app  <$@> ef <*@> ea
    Cnd ec et ef       -> FAV.cnd  <$@> ec <*@> et <*@> ef
    Whl xc ec xb eb ei -> FAV.whl  <$@> (xc , ec) <*@> (xb , eb) <*@> ei
    Tpl ef es          -> FAV.tpl  <$@> ef <*@> es
    Fst e              -> FAV.fst  <$@> e
    Snd e              -> FAV.snd  <$@> e
    Ary el x ef        -> FAV.ary  <$@> el <*@> (x , ef)
    Len e              -> FAV.len  <$@> e
    Ind ea ei          -> FAV.ind  <$@> ea <*@> ei
    Let x el eb        -> pure     <$@> App (Abs x eb) el
    Cmx er ei          -> FAV.cmx  <$@> er <*@> ei)

instance Eq v => Cnv ((v , Exp v) , Env v FAV.Exp) (FAV.Exp -> FAV.Exp) where
  cnv ((x , e) , r) = pure (frmRgt . curry cnv e . (: r) . (,) x)