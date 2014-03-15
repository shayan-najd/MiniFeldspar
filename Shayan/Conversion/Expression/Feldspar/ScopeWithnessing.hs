module Conversion.Expression.Feldspar.ScopeWithnessing () where

import Prelude ()
import MyPrelude

import qualified Expression.Feldspar.ADTUntypedDebruijn  as FAUD
import qualified Expression.Feldspar.GADTUntypedDebruijn as FGUD
 
import Nat.GADT 

import Conversion
import Conversion.Nat           ()
import Conversion.Type.Feldspar ()
import Conversion.Variable      ()
 
instance Cnv FAUD.Exp (Exs1 FGUD.Exp Nat) where
  cnv ee = do let n = maximum (FAUD.fre ee)
              ExsSin n' :: ExsSin Nat <- cnv n 
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
         