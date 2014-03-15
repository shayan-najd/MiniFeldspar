module Conversion.Expression.Feldspar.Evaluation.GADTHigherOrder () where

import Prelude ()
import MyPrelude

import Expression.Feldspar.GADTHigherOrder 
import qualified Expression.Feldspar.GADTValue as V

import qualified Type.Feldspar.ADT  as TFA
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
    
instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb , ta' ~ ta , tb' ~ tb) => 
         Cnv (Exp r ta -> Exp r tb , Env V.Val r) (V.Val (TFA.Arr ta' tb')) where
  cnv (f , r)  =  pure (V.Val ( V.getVal 
                               . frmRgt . flip (curry cnv) r 
                               . f 
                               . frmRgt . flip (curry cnv) r 
                               . V.Val ))
         
instance (HasSin TFG.Typ t , r ~ r' , t ~ t') => 
         Cnv (V.Val t' , Env V.Val r') (Exp r t)
         where
  cnv (V.Val v , r) = let ?r = r in case Singleton.sin :: TFG.Typ t of
    TFG.Int                   -> ConI <$@> v
    TFG.Bol                   -> ConB <$@> v
    TFG.Arr (_ :: TFG.Typ ta) 
            (_ :: TFG.Typ tb) -> case TFG.getPrfHasSinArr (T :: T t) of
     (PrfHasSin , PrfHasSin)  -> Abs  <$@> (V.Val v :: V.Val (TFA.Arr ta tb)) 
    TFG.Tpl _ _               -> case TFG.getPrfHasSinTpl (T :: T t) of
     (PrfHasSin , PrfHasSin)  -> Tpl  <$@> V.Val (fst v) <*@> V.Val (snd v) 
    TFG.Ary (_ :: TFG.Typ ta) -> case TFG.getPrfHasSinAry (T :: T t) of
      PrfHasSin                
        | fst (bounds v) == 0 -> Ary  <$> (ConI <$@> (snd . bounds) v) 
                                      <*@> ((V.Val (fromJust 
                                                    . flip lookup (assocs v))) 
                                            :: V.Val (TFA.Arr TFA.Int ta))
        | otherwise           -> fail "Bad Array!"                         
  
instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb , r ~ r' , ta ~ ta' , tb ~ tb')=>
         Cnv (V.Val (TFA.Arr ta' tb') , Env V.Val r') (Exp r ta -> Exp r tb)
         where
  cnv (V.Val f , r) = pure ( frmRgt  . flip (curry cnv) r 
                           . V.Val   . f . V.getVal 
                           . frmRgt  . flip (curry cnv) r)         