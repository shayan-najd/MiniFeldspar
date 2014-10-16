module Expression.Feldspar.Conversions.Lifting () where

import MyPrelude

import qualified Expression.Feldspar.GADTFirstOrder  as FGFO
import qualified Expression.Feldspar.GADTHigherOrder as FGHO

import qualified Type.Feldspar.GADT                  as TFG

import Variable.Typed    as VT
import Environment.Typed as ET

import Conversion
import Variable.Conversion ()

instance (t ~ t' , r ~ r') =>
         Cnv (FGFO.Exp r t , Env TFG.Typ r) (FGHO.Exp r' t') where
  cnv (e , r) = cnv (e , (ET.fmap FGHO.Var . cnvGEnvtoGVar) r)

cnvGEnvtoGVar ::  Env tf r -> Env (VT.Var r) r
cnvGEnvtoGVar ET.Emp        = ET.Emp
cnvGEnvtoGVar (ET.Ext _ xs) = ET.Ext VT.Zro (ET.fmap VT.Suc (cnvGEnvtoGVar xs))

instance (t ~ t' , r ~ r') =>
         Cnv (FGFO.Exp r t , Env (FGHO.Exp r) r) (FGHO.Exp r' t') where
  cnv (ee , r) = let ?r = r in case ee of
      FGFO.ConI i       -> pure (FGHO.ConI i)
      FGFO.ConB b       -> pure (FGHO.ConB b)
      FGFO.ConF f       -> pure (FGHO.ConF f)
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
      FGFO.Cmx er ei    -> FGHO.Cmx  <$@> er <*@> ei
      FGFO.Non          -> pure FGHO.Non
      FGFO.Som e        -> FGHO.Som  <$@> e
      FGFO.May em en es -> FGHO.May  <$@> em <*@> en <*@> es

instance (ta ~ ta' , tb ~ tb' , r ~ r') =>
         Cnv (FGFO.Exp (ta ': r) tb , Env (FGHO.Exp r) r)
             (FGHO.Exp r' ta' -> FGHO.Exp r' tb')
         where
  cnv (eb , r) = pure (FGHO.prdAll
                      . frmRgt . cnv' eb
                      . ET.fmap FGHO.sucAll
                      . flip Ext r)
    where
      cnv' :: forall rr tt. FGFO.Exp rr tt  -> Env (FGHO.Exp rr) rr ->
              ErrM (FGHO.Exp rr tt)
      cnv' = curry cnv