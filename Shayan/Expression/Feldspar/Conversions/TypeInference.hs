module Expression.Feldspar.Conversions.TypeInference () where

import MyPrelude

import qualified Expression.Feldspar.GADTUntypedDebruijn as FGUD
import qualified Expression.Feldspar.GADTTyped           as FGTD

import qualified Type.Herbrand                           as TH
import qualified Type.Feldspar.ADT                       as TFA

import Environment.Scoped

import Conversion
import Variable.Conversion   ()

import Inference             (typInf)
import TypeChecking.Feldspar ()


instance n ~ n' => Cnv (FGUD.Exp n , Env n TFA.Typ) (FGTD.Exp n' TFA.Typ) where
  cnv (e , r) = let ?r = r in
                do e' :: FGTD.Exp n () <- cnvImp e
                   cnvImp e'

instance n ~ n' => Cnv (FGUD.Exp n , rr) (FGTD.Exp n' ()) where
  cnv (eaum , r) = let ?r = r in case eaum of
    FGUD.ConI i       -> pure (FGTD.ConI i)
    FGUD.ConB b       -> pure (FGTD.ConB b)
    FGUD.ConF f       -> pure (FGTD.ConF f)
    FGUD.Var v        -> FGTD.Var  <$@> v
    FGUD.Abs eb       -> FGTD.Abs  <$@> eb
    FGUD.App ef ea    -> FGTD.App  <$@> () <*@> ef <*@> ea
    FGUD.Cnd ec et ef -> FGTD.Cnd  <$@> ec <*@> et <*@> ef
    FGUD.Whl ec eb ei -> FGTD.Whl  <$@> ec <*@> eb <*@> ei
    FGUD.Tpl ef es    -> FGTD.Tpl  <$@> ef <*@> es
    FGUD.Fst e        -> FGTD.Fst  <$@> () <*@> e
    FGUD.Snd e        -> FGTD.Snd  <$@> () <*@> e
    FGUD.Ary el ef    -> FGTD.Ary  <$@> el <*@> ef
    FGUD.Len e        -> FGTD.Len  <$@> () <*@> e
    FGUD.Ind ea ei    -> FGTD.Ind  <$@> ea <*@> ei
    FGUD.Let el eb    -> FGTD.Let  <$@> () <*@> el <*@> eb
    FGUD.Cmx er ei    -> FGTD.Cmx  <$@> er <*@> ei
    FGUD.Non          -> pure FGTD.Non
    FGUD.Som e        -> FGTD.Som  <$@> e
    FGUD.May em en es -> FGTD.May  <$@> () <*@> em <*@> en <*@> es

instance n ~ n' => Cnv (FGTD.Exp n () , Env n TFA.Typ)(FGTD.Exp n' TFA.Typ) where
  cnv (e , r) = let ?r = r in
    do r' :: Env n (TH.Typ (TH.EnvFld '[])) <- cnvImp r
       e' <- typInf e r'
       traverse cnvImp e'

instance n ~ n' => Cnv (FGTD.Exp n' t , r) (FGUD.Exp n)  where
  cnv (eaum , r) = let ?r = r in case eaum of
    FGTD.ConI i         -> pure (FGUD.ConI i)
    FGTD.ConB b         -> pure (FGUD.ConB b)
    FGTD.ConF f         -> pure (FGUD.ConF f)
    FGTD.Var v          -> FGUD.Var  <$@> v
    FGTD.Abs eb         -> FGUD.Abs  <$@> eb
    FGTD.App _ ef ea    -> FGUD.App  <$@> ef <*@> ea
    FGTD.Cnd ec et ef   -> FGUD.Cnd  <$@> ec <*@> et <*@> ef
    FGTD.Whl ec eb ei   -> FGUD.Whl  <$@> ec <*@> eb <*@> ei
    FGTD.Tpl ef es      -> FGUD.Tpl  <$@> ef <*@> es
    FGTD.Fst _ e        -> FGUD.Fst  <$@> e
    FGTD.Snd _ e        -> FGUD.Snd  <$@> e
    FGTD.Ary el ef      -> FGUD.Ary  <$@> el <*@> ef
    FGTD.Len _ e        -> FGUD.Len  <$@> e
    FGTD.Ind ea ei      -> FGUD.Ind  <$@> ea <*@> ei
    FGTD.Let _ el eb    -> FGUD.Let  <$@> el <*@> eb
    FGTD.Cmx er ei      -> FGUD.Cmx  <$@> er <*@> ei
    FGTD.Non            -> pure FGUD.Non
    FGTD.Som e          -> FGUD.Som  <$@> e
    FGTD.May _ em en es -> FGUD.May  <$@> em <*@> en <*@> es