module Expression.Feldspar.Conversions.Evaluation.GADTFirstOrder () where

import MyPrelude

import Expression.Feldspar.GADTFirstOrder
import qualified Expression.Feldspar.GADTValue as FGV

import qualified Type.Feldspar.ADT  as TFA
import qualified Type.Feldspar.GADT as TFG

import Environment.Typed

import Conversion
import Variable.Conversion ()

import Singleton

instance (HasSin TFG.Typ t , t' ~ t) =>
         Cnv (Exp r t , Env FGV.Exp r) (FGV.Exp t') where
  cnv (ee , r) = let ?r = r in case ee of
    ConI i                   -> FGV.conI <$@> i
    ConB b                   -> FGV.conB <$@> b
    ConF b                   -> FGV.conF <$@> b
    Var x                    -> FGV.var  <$@> x
    Abs eb                   -> case TFG.getPrfHasSinArr (T :: T t) of
     (PrfHasSin , PrfHasSin) -> cnv (eb , r)
    App ef ea                -> FGV.app  <$@> ef <*@> ea
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
    Let el eb                -> cnv (App (Abs eb) el , r)
    Cmx er ei                -> FGV.cmx  <$@> er <*@> ei

instance (ta' ~ ta , tb' ~ tb , HasSin TFG.Typ ta , HasSin TFG.Typ tb) =>
         Cnv (Exp (ta ': r) tb , Env FGV.Exp r)  (FGV.Exp (TFA.Arr ta' tb'))
         where
  cnv  (e , r) = (pure . FGV.Exp)
                  (FGV.getTrm . frmRgt . curry cnv e
                   . flip Ext r . (FGV.Exp :: Trm ta -> FGV.Exp ta))

{-
instance (HasSin TFG.Typ t , r ~ r' , t ~ t') =>
         Cnv (FGV.Exp t', Env FGV.Exp r') (Exp r t) where
  cnv (v , r) = case sin :: TFG.Typ t of
    TFG.Int       -> pure (ConI (FGV.getVal v))
    TFG.Bol       -> pure (ConB (FGV.getVal v))
    TFG.Arr ta tb -> case (getPrfHasSin ta , getPrfHasSin tb) of
      (PrfHasSin , PrfHasSin) -> return (Abs undefined)
    TFG.Tpl tf ts -> case (getPrfHasSin tf , getPrfHasSin ts) of
      (PrfHasSin , PrfHasSin) -> do
        ef <- cnv (FGV.Exp $ fst (FGV.getVal v) , r)
        es <- cnv (FGV.Exp $ snd (FGV.getVal v) , r)
        pure (Tpl ef es)
    TFG.Ary (ta :: TFG.Typ ta)-> case getPrfHasSin ta of
      PrfHasSin -> let (0,l) = bounds (FGV.getVal v) in
        return (Ary (ConI l) undefined)
-}