module Conversion.Expression.Feldspar.Evaluation.MiniWellScoped () where

import MyPrelude

import Expression.Feldspar.MiniWellScoped
import qualified Expression.Feldspar.GADTValue as FGV

import qualified Type.Feldspar.ADT             as TFA
import qualified Type.Feldspar.GADT            as TFG

import Variable.Typed
import Environment.Typed

import Conversion
import Conversion.Variable ()

import Singleton

instance (HasSin TFG.Typ t, r' ~ r , t' ~ t) =>
         Cnv (Exp r t , Env FGV.Exp r) (FGV.Exp t')
         where
  cnv (egfo , r) = let ?r = r in case egfo of
    ConI i                   -> FGV.conI <$@> i
    ConB b                   -> FGV.conB <$@> b
    ConF b                   -> FGV.conF <$@> b
    AppV (v :: Var rv tv) es ->   appV (T :: T tv) <$@> v <*@> (T :: T tv , es)
    Cnd ec et ef             -> FGV.cnd  <$@> ec <*@> et <*@> ef
    Whl ec eb ei             -> FGV.whl  <$@> ec <*@> eb <*@> ei
    Tpl ef es                -> case TFG.getPrfHasSinTpl (T :: T t) of
     (PrfHasSin , PrfHasSin) -> FGV.tpl  <$@> ef <*@> es
    Fst e                    -> FGV.fst  <$@> e
    Snd e                    -> FGV.snd  <$@> e
    Ary el ef                -> case TFG.getPrfHasSinAry (T :: T t) of
     PrfHasSin               -> FGV.ary  <$@> el <*@> ef
    Len e                    -> FGV.len  <$@> e
    Ind ea ei                -> FGV.ind  <$@> ea <*@> ei
    Let el eb                -> FGV.lett <$@> el <*@> eb
    Cmx er ei                -> FGV.cmx  <$@> er <*@> ei
    Tmp _                    -> fail "Not Supported!"

instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb , ta' ~ ta , tb' ~ tb) =>
         Cnv (Exp r ta -> Exp r tb , Env FGV.Exp r) (FGV.Exp (TFA.Arr ta' tb'))
         where
  cnv (f , r)  =  let ?r = r in pure (FGV.Exp (FGV.getTrm
                                             . frmRgt . cnvImp
                                             . f
                                             . frmRgt . cnvImp
                                             . FGV.Exp ))

instance (HasSin TFG.Typ t , t' ~ TFG.Arg t) =>
         Cnv ((T t , Env (Exp r) t') , Env FGV.Exp r) (Env FGV.Exp   t')
         where
  cnv ((T , vss) , r) = let ?r = r in case (Singleton.sin :: TFG.Typ t , vss) of
    (TFG.Arr ta tb , Ext v vs) -> case TFG.getPrfHasSinArr (T :: T t) of
      (PrfHasSin , PrfHasSin)  -> Ext <$@> samTyp ta v <*@> (samTyp tb T , vs)
    (TFG.Arr _ _   , _)        -> impossibleM
    (TFG.Int       , Emp)      -> pure Emp
    (TFG.Int       , _)        -> impossibleM
    (TFG.Bol       , Emp)      -> pure Emp
    (TFG.Bol       , _)        -> impossibleM
    (TFG.Flt       , Emp)      -> pure Emp
    (TFG.Flt       , _)        -> impossibleM
    (TFG.Tpl _  _  , Emp)      -> pure Emp
    (TFG.Tpl _  _  , _)        -> impossibleM
    (TFG.Ary _     , Emp)      -> pure Emp
    (TFG.Ary _     , _)        -> impossibleM
    (TFG.Cmx       , Emp)      -> pure Emp
    (TFG.Cmx       , _)        -> impossibleM

appV :: forall t. HasSin TFG.Typ t => T t ->
          FGV.Exp t -> Env FGV.Exp (TFG.Arg t) -> FGV.Exp (TFG.Out t)
appV T vv vss  = case (Singleton.sin :: TFG.Typ t , vss) of
  (TFG.Arr _ tb , Ext v vs) -> case TFG.getPrfHasSinArr (T :: T t) of
    (PrfHasSin , PrfHasSin) -> appV T (samTyp tb (FGV.app vv v)) vs
  (TFG.Int       , Emp)     -> vv
  (TFG.Bol       , Emp)     -> vv
  (TFG.Flt       , Emp)     -> vv
  (TFG.Tpl _  _  , Emp)     -> vv
  (TFG.Ary _     , Emp)     -> vv
  (TFG.Cmx       , Emp)     -> vv

instance (HasSin TFG.Typ t , r ~ r' , t ~ t') =>
         Cnv (FGV.Exp t' , Env FGV.Exp r') (Exp r t)
         where
  cnv (FGV.Exp v , r) = let ?r = r in case Singleton.sin :: TFG.Typ t of
    TFG.Int                   -> ConI <$@> v
    TFG.Bol                   -> ConB <$@> v
    TFG.Flt                   -> ConF <$@> v
    TFG.Tpl _ _               -> case TFG.getPrfHasSinTpl (T :: T t) of
     (PrfHasSin , PrfHasSin)  -> Tpl  <$@> FGV.Exp (fst v) <*@> FGV.Exp (snd v)
    TFG.Ary ta                -> case TFG.getPrfHasSinAry (T :: T t) of
      PrfHasSin
        | fst (bounds v) == 0 -> Ary  <$@> (FGV.Exp . (+ 1) . snd . bounds) v
                                      <*@> samTyp (TFG.Arr TFG.Int ta)
                                            (FGV.Exp (fromJust
                                                    . flip lookup (assocs v)))
        | otherwise           -> fail "Bad Array!"
    TFG.Cmx                   -> Cmx <$@> FGV.Exp (realPart v)
                                     <*@> FGV.Exp (imagPart v)
    TFG.Arr _ _               -> fail "Type Error!"

instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb , r ~ r' , ta ~ ta' , tb ~ tb')=>
         Cnv (FGV.Exp (TFA.Arr ta' tb') , Env FGV.Exp r') (Exp r ta -> Exp r tb)
         where
  cnv (FGV.Exp f , r) = let ?r = r in
    pure ( frmRgt  . cnvImp
         . FGV.mapTrm  f
         . frmRgt  . cnvImp)