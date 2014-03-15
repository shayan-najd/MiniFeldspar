module Conversion.Expression.Feldspar.Evaluation.GADTFirstOrder () where

import Prelude ()
import MyPrelude

import Expression.Feldspar.GADTFirstOrder
import qualified Expression.Feldspar.GADTValue as V

import qualified Type.Feldspar.ADT as TFA
import qualified Type.Feldspar.GADT as TFG
 
import Environment.Typed   

import Conversion
import Conversion.Variable ()

import Singleton 

instance (HasSin TFG.Typ t , t' ~ t) => 
         Cnv (Exp r t , Env V.Val r) (V.Val t') where
  cnv (ee , r) = let ?r = r in case ee of 
    ConI i                   -> V.conI <$@> i
    ConB b                   -> V.conB <$@> b
    Var x                    -> V.var  <$@> x
    Abs eb                   -> case TFG.getPrfHasSinArr (T :: T t) of
     (PrfHasSin , PrfHasSin) -> cnv (eb , r)
    App ef ea                -> V.app  <$@> ef <*@> ea
    Cnd ec et ef             -> V.cnd  <$@> ec <*@> et <*@> ef
    Whl ec eb ei             -> V.whl  <$@> ec <*@> eb <*@> ei
    Tpl ef es                -> case TFG.getPrfHasSinTpl (T :: T t) of
     (PrfHasSin , PrfHasSin) -> V.tpl  <$@> ef <*@> es
    Fst e                    -> V.fst  <$@> e
    Snd e                    -> V.snd  <$@> e                      
    Ary el ef                -> case TFG.getPrfHasSinAry (T :: T t) of
     PrfHasSin               -> V.ary  <$@> el <*@> ef 
    Len e                    -> V.len  <$@> e                        
    Ind ea ei                -> V.ind  <$@> ea <*@> ei
    Let el eb                -> cnv (App (Abs eb) el , r)
 
instance (ta' ~ ta , tb' ~ tb , HasSin TFG.Typ ta , HasSin TFG.Typ tb) => 
         Cnv (Exp (ta ': r) tb , Env V.Val r)  (V.Val (TFA.Arr ta' tb'))
         where
  cnv  (e , r) = (pure . V.Val) 
                  (V.getVal . frmRgt . curry cnv e 
                   . flip Ext r . (V.Val :: Trm ta -> V.Val ta))                 

{-  
instance (HasSin TFG.Typ t , r ~ r' , t ~ t') => 
         Cnv (V.Val t', Env V.Val r') (Exp r t) where
  cnv (v , r) = case sin :: TFG.Typ t of
    TFG.Int       -> pure (ConI (V.getVal v))
    TFG.Bol       -> pure (ConB (V.getVal v))
    TFG.Arr ta tb -> case (getPrfHasSin ta , getPrfHasSin tb) of 
      (PrfHasSin , PrfHasSin) -> return (Abs undefined)
    TFG.Tpl tf ts -> case (getPrfHasSin tf , getPrfHasSin ts) of 
      (PrfHasSin , PrfHasSin) -> do 
        ef <- cnv (V.Val $ fst (V.getVal v) , r)
        es <- cnv (V.Val $ snd (V.getVal v) , r)
        pure (Tpl ef es)
    TFG.Ary (ta :: TFG.Typ ta)-> case getPrfHasSin ta of
      PrfHasSin -> let (0,l) = bounds (V.getVal v) in
        return (Ary (ConI l) undefined)
-} 