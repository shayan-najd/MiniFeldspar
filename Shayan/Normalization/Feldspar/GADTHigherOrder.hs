module Normalization.Feldspar.GADTHigherOrder () where

import Prelude ()
import MyPrelude

import Expression.Feldspar.GADTHigherOrder
   
import Normalization

instance NrmOne (Exp n t) where
  nrmOne ee = case ee of
    ConI i           -> ConI <$@> i 
    ConB b           -> ConB <$@> b
    Var x            -> Var  <$@> x
    Abs eb           -> Abs  <$@> eb
    App (Abs eb) ea  -> chg  (eb ea)
    App ef       ea  -> App  <$@> ef <*@> ea    
    Cnd ec et ef     -> Cnd  <$@> ec <*@> et <*@> ef
    Whl ec eb ei     -> Whl  <$@> ec <*@> eb <*@> ei
    Tpl ef es        -> Tpl  <$@> ef <*@> es
    Fst e            -> Fst  <$@> e
    Snd e            -> Snd  <$@> e                       
    Ary el ef        -> Ary  <$@> el <*@> ef
    Len e            -> Len  <$@> e                         
    Ind ea ei        -> Ind  <$@> ea <*@> ei                         
    Let el eb        -> Let  <$@> el <*@> eb
    
instance NrmOne (Exp n ta -> Exp n tb) where
  nrmOne = pure