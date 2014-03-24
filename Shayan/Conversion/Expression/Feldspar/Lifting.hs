module Conversion.Expression.Feldspar.Lifting () where

import Prelude ()
import MyPrelude 

import qualified Expression.Feldspar.GADTFirstOrder  as FGFO
import qualified Expression.Feldspar.GADTHigherOrder as FGHO 

import qualified Type.Feldspar.GADT                  as TFG

import Environment.Typed      
 
import Conversion 
import Conversion.Variable ()
   
instance (t ~ t' , r ~ r') => 
         Cnv (FGFO.Exp r t , Env TFG.Typ r) (FGHO.Exp r' t') where
  cnv (e , r) = cnv (e , (map FGHO.Var . cnvGEnvtoGVar) r)

instance (t ~ t' , r ~ r') => 
         Cnv (FGFO.Exp r t , Env (FGHO.Exp r) r) (FGHO.Exp r' t') where
  cnv (ee , r) = let ?r = r in case ee of  
      FGFO.ConI i       -> FGHO.ConI <$@> i
      FGFO.ConB b       -> FGHO.ConB <$@> b
      FGFO.Var v        -> id        <$@> v 
      FGFO.Abs eb       -> FGHO.Abs  <$@> eb
      FGFO.App ef ea    -> FGHO.App  <$@> ef <*@> ea
      FGFO.Cnd ec et ef -> FGHO.Cnd  <$@> ec <*@> et <*@> ef
      FGFO.Whl ec eb ei -> FGHO.Whl  <$@> ec <*@> eb <*@> ei
      FGFO.Tpl ef es    -> FGHO.Tpl  <$@> ef <*@> es
      FGFO.Fst e        -> FGHO.Fst  <$@> e
      FGFO.Snd e        -> FGHO.Snd  <$@> e
      FGFO.Ary el ef    -> FGHO.Ary  <$@> el <*@> ef
      FGFO.Len e        -> FGHO.Len  <$@> e 
      FGFO.Ind ea ei    -> FGHO.Ind  <$@> ea <*@> ei
      FGFO.Let el eb    -> FGHO.Let  <$@> el <*@> eb  
       
instance (ta ~ ta' , tb ~ tb' , r ~ r') => 
         Cnv (FGFO.Exp (ta ': r) tb , Env (FGHO.Exp r) r) 
             (FGHO.Exp r' ta' -> FGHO.Exp r' tb') 
         where
  cnv (eb , r) = pure (FGHO.prdAll  
                      . frmRgt . cnv' eb
                      . map FGHO.sucAll 
                      . flip Ext r) 
    where
      cnv' :: forall rr tt. FGFO.Exp rr tt  -> Env (FGHO.Exp rr) rr -> 
              ErrM (FGHO.Exp rr tt)
      cnv' = curry cnv