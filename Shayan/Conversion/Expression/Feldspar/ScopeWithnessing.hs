module Conversion.Expression.Feldspar.ScopeWithnessing () where

import Prelude ()
import MyPrelude

import qualified Expression.Feldspar.ADTUntypedDebruijn  as FAUD
import qualified Expression.Feldspar.GADTUntypedDebruijn as FGUD
 
import Nat.GADT 

import Conversion
import Conversion.Variable ()
 
instance Cnv (FAUD.Exp , r) (Exs1 FGUD.Exp Nat) where
  cnv (ee , r)  = let ?r = r in  
    do let n = maximum (FAUD.fre ee)
       ExsSin n' :: ExsSin Nat <- cnvImp n 
       e' <- cnv (ee , n')
       return (Exs1 e' n')
     
instance n ~ n' => Cnv (FAUD.Exp , Nat n) (FGUD.Exp n') where
  cnv (eaum  , n) = let ?r = n in case eaum of
    FAUD.ConI i       -> FGUD.ConI <$@> i
    FAUD.ConB b       -> FGUD.ConB <$@> b 
    FAUD.Var v        -> FGUD.Var  <$@> v
    FAUD.Abs eb       -> FGUD.Abs  <$> cnvf eb 
    FAUD.App ef ea    -> FGUD.App  <$@> ef <*@> ea
    FAUD.Cnd ec et ef -> FGUD.Cnd  <$@> ec <*@> et <*@> ef 
    FAUD.Whl ec eb ei -> FGUD.Whl  <$> cnvf ec <*> cnvf eb <*@> ei
    FAUD.Tpl ef es    -> FGUD.Tpl  <$@> ef <*@> es
    FAUD.Fst e        -> FGUD.Fst  <$@> e
    FAUD.Snd e        -> FGUD.Snd  <$@> e
    FAUD.Ary el ef    -> FGUD.Ary  <$@> el <*> cnvf ef    
    FAUD.Len e        -> FGUD.Len  <$@> e 
    FAUD.Ind ea ei    -> FGUD.Ind  <$@> ea <*@> ei
    FAUD.Let el eb    -> FGUD.Let  <$@> el <*> cnvf eb
    where
      cnvf e = cnv (e , Suc n) 
         
instance Cnv (FGUD.Exp n , r) (FAUD.Exp) where
  cnv (eaum  , r) = let ?r = r in case eaum of
    FGUD.ConI i       -> FAUD.ConI <$@> i
    FGUD.ConB b       -> FAUD.ConB <$@> b 
    FGUD.Var v        -> FAUD.Var  <$@> v
    FGUD.Abs eb       -> FAUD.Abs  <$@> eb 
    FGUD.App ef ea    -> FAUD.App  <$@> ef <*@> ea
    FGUD.Cnd ec et ef -> FAUD.Cnd  <$@> ec <*@> et <*@> ef 
    FGUD.Whl ec eb ei -> FAUD.Whl  <$@> ec <*@> eb <*@> ei
    FGUD.Tpl ef es    -> FAUD.Tpl  <$@> ef <*@> es
    FGUD.Fst e        -> FAUD.Fst  <$@> e
    FGUD.Snd e        -> FAUD.Snd  <$@> e
    FGUD.Ary el ef    -> FAUD.Ary  <$@> el <*@> ef    
    FGUD.Len e        -> FAUD.Len  <$@> e 
    FGUD.Ind ea ei    -> FAUD.Ind  <$@> ea <*@> ei
    FGUD.Let el eb    -> FAUD.Let  <$@> el <*@> eb
 
