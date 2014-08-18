module Conversion.Expression.Feldspar.Evaluation.GADTHigherOrder () where

import MyPrelude

import Expression.Feldspar.GADTHigherOrder
import qualified Expression.Feldspar.GADTValue as FGV

import qualified Type.Feldspar.ADT  as TFA
import qualified Type.Feldspar.GADT as TFG

import Environment.Typed

import Conversion
import Conversion.Variable ()

import Singleton

instance (HasSin TFG.Typ t , t' ~ t) =>
         Cnv (Exp r t , Env FGV.Exp r) (FGV.Exp t') where
  cnv (ee , r) = let ?r = r in let t = sin :: TFG.Typ t in case ee of
    ConI i                   -> FGV.conI <$@> i
    ConB b                   -> FGV.conB <$@> b
    ConF b                   -> FGV.conF <$@> b
    Var x                    -> FGV.var  <$@> x
    Abs eb                   -> case TFG.getPrfHasSinArr t of
     (PrfHasSin , PrfHasSin) -> cnvImp eb
    App ef ea                -> FGV.app  <$@> ef <*@> ea
    Cnd ec et ef             -> FGV.cnd  <$@> ec <*@> et <*@> ef
    Whl ec eb ei             -> FGV.whl  <$@> ec <*@> eb <*@> ei
    Tpl ef es                -> case TFG.getPrfHasSinTpl t of
     (PrfHasSin , PrfHasSin) -> FGV.tpl  <$@> ef <*@> es
    Fst e                    -> FGV.fst  <$@> e
    Snd e                    -> FGV.snd  <$@> e
    Ary el ef                -> case TFG.getPrfHasSinAry t of
     PrfHasSin               -> FGV.ary  <$@> el <*@> ef
    Len e                    -> FGV.len  <$@> e
    Ind ea ei                -> FGV.ind  <$@> ea <*@> ei
    Let el eb                -> cnvImp (App (Abs eb) el)
    Cmx er ei                -> FGV.cmx  <$@> er <*@> ei
    Tmp _                    -> fail "Not Supported!"

instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb , ta' ~ ta , tb' ~ tb) =>
         Cnv (Exp r ta -> Exp r tb , Env FGV.Exp r) (FGV.Exp (TFA.Arr ta' tb')) where
  cnv (f , r)  =  let ?r = r in
    pure (FGV.Exp ( FGV.getTrm
                  . frmRgt . cnvImp
                  . f
                  . frmRgt . cnvImp
                  . FGV.Exp ))

instance (HasSin TFG.Typ t , r ~ r' , t ~ t') =>
         Cnv (FGV.Exp t' , Env FGV.Exp r') (Exp r t)
         where
  cnv (FGV.Exp v , r) = let ?r = r in
    let t = Singleton.sin :: TFG.Typ t in case t of
    TFG.Int                   -> ConI <$@> v
    TFG.Bol                   -> ConB <$@> v
    TFG.Flt                   -> ConF <$@> v
    TFG.Arr _ _               -> case TFG.getPrfHasSinArr t of
     (PrfHasSin , PrfHasSin)  -> Abs  <$@> samTyp t (FGV.Exp v)
    TFG.Tpl _ _               -> case TFG.getPrfHasSinTpl t of
     (PrfHasSin , PrfHasSin)  -> Tpl  <$@> FGV.Exp (fst v) <*@> FGV.Exp (snd v)
    TFG.Ary ta                -> case TFG.getPrfHasSinAry t of
      PrfHasSin
        | fst (bounds v) == 0 -> Ary  <$@> (FGV.Exp . (+ 1) . snd . bounds) v
                                      <*@> (samTyp (TFG.Arr TFG.Int ta)
                                            (FGV.Exp (fromJust
                                                    . flip lookup (assocs v))))
        | otherwise           -> fail "Bad Array!"
    TFG.Cmx                   -> Cmx <$@> FGV.Exp (realPart v)
                                     <*@> FGV.Exp (imagPart v)

instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb , r ~ r' , ta ~ ta' , tb ~ tb')=>
         Cnv (FGV.Exp (TFA.Arr ta' tb') , Env FGV.Exp r') (Exp r ta -> Exp r tb)
         where
  cnv (FGV.Exp f , r) = let ?r = r in
    pure (frmRgt . cnvImp . FGV.mapTrm f . frmRgt . cnvImp)