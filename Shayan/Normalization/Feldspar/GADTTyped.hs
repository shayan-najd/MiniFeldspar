module Normalization.Feldspar.GADTTyped () where

import Prelude ()
import MyPrelude 

import Expression.Feldspar.GADTTyped

import Variable.Scoped

import Normalization
   
instance NrmOne (Exp n t) where
  nrmOne ee = case ee of
    ConI i              -> ConI <$@> i 
    ConB b              -> ConB <$@> b
    ConF b              -> ConF <$@> b    
    Var x               -> Var  <$@> x
    Abs eb              -> Abs  <$@> eb
    App _  (Abs eb) ea  -> chg ((prdAll . sbs eb Zro . sucAll) ea)
    App t  ef       ea  -> App  <$> pure t <*@> ef <*@> ea    
    Cnd ec et ef        -> Cnd  <$@> ec    <*@> et <*@> ef
    Whl ec eb ei        -> Whl  <$@> ec    <*@> eb <*@> ei
    Tpl ef es           -> Tpl  <$@> ef    <*@> es
    Fst t  e            -> Fst  <$> pure t <*@> e
    Snd t  e            -> Snd  <$> pure t <*@> e                       
    Ary el ef           -> Ary  <$@> el    <*@> ef
    Len t  e            -> Len  <$> pure t <*@> e                         
    Ind ea ei           -> Ind  <$@> ea    <*@> ei                         
    Let t  el eb        -> Let  <$> pure t <*@> el <*@> eb
    Cmx er ei           -> Cmx  <$@> er    <*@> ei                             