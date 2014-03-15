module Conversion.Expression.Feldspar.TypeWithnessing () where

import Prelude ()
import MyPrelude

import qualified Expression.Feldspar.GADTTyped      as FGTD
import qualified Expression.Feldspar.GADTFirstOrder as FGFO

import qualified Type.Feldspar.GADT                 as TFG
import qualified Type.Feldspar.ADT                  as TFA

import Environment.Typed              

import Conversion
import Conversion.Type.Feldspar ()
import Conversion.Variable      ()

import Singleton
  
instance (r ~ r' , n ~ Len r , HasSin TFG.Typ t) =>
         Cnv (FGTD.Exp n TFA.Typ , Env TFG.Typ r)  
             (FGFO.Exp r' t) where
  cnv (ee , r) = let ?r = r in case (ee , sin :: TFG.Typ t) of
    (FGTD.ConI i       , TFG.Int)       -> 
      FGFO.ConI <$@> i
    (FGTD.ConB b       , TFG.Bol)       -> 
      FGFO.ConB <$@> b
    (FGTD.Var x        , _)            -> 
      FGFO.Var  <$@> x
    (FGTD.Abs eb       , TFG.Arr ta tb) -> do 
      PrfHasSin <- getPrfHasSinM ta
      PrfHasSin <- getPrfHasSinM tb 
      FGFO.Abs <$> cnv (eb , Ext ta r) 
    (FGTD.App ta ef ea , _)            -> do 
      ExsSin (ta' :: TFG.Typ tt) <- cnv ta
      PrfHasSin <- getPrfHasSinM ta'
      ea' :: FGFO.Exp r' tt <- cnv (ea , r)
      FGFO.App <$@> ef <*> pure ea' 
    (FGTD.Cnd ec et ef , _)            -> 
      FGFO.Cnd <$@> ec <*@> et <*@> ef 
    (FGTD.Whl ec eb ei , _)            -> 
      FGFO.Whl 
      <$> cnv (ec , Ext (sin :: TFG.Typ t) r)
      <*> cnv (eb , Ext (sin :: TFG.Typ t) r) 
      <*@> ei
    (FGTD.Tpl ef es    , TFG.Tpl tf ts) -> do 
      PrfHasSin <- getPrfHasSinM tf
      PrfHasSin <- getPrfHasSinM ts
      FGFO.Tpl <$@> ef <*@> es
    (FGTD.Fst ts e     , _)            -> do 
      ExsSin (ts' :: TFG.Typ tt)        <- cnv ts
      PrfHasSin <- getPrfHasSinM ts' 
      e' :: FGFO.Exp r' (TFA.Tpl t tt) <- cnv (e , r)
      FGFO.Fst <$> pure e' 
    (FGTD.Snd tf e     , _)            -> do 
      ExsSin (tf' :: TFG.Typ tt)        <- cnv tf
      PrfHasSin <- getPrfHasSinM tf'
      e' :: FGFO.Exp r' (TFA.Tpl tt t) <- cnv (e , r)
      FGFO.Snd <$> pure e'
    (FGTD.Ary el ef    , TFG.Ary ta)    -> do 
      PrfHasSin <- getPrfHasSinM ta
      FGFO.Ary <$@> el <*> cnv (ef , Ext TFG.Int r)
    (FGTD.Len ta e     , TFG.Int )      -> do 
      ExsSin (ta' :: TFG.Typ tt)      <- cnv ta
      PrfHasSin <- getPrfHasSinM ta'
      e' :: FGFO.Exp r' (TFA.Ary tt) <- cnv (e , r)
      FGFO.Len <$> pure e'
    (FGTD.Ind e  ei , _)               -> 
      FGFO.Ind <$@> e <*@> ei
    (FGTD.Let tl el eb , _)            -> do 
      ExsSin tl' :: ExsSin TFG.Typ <- cnv tl
      PrfHasSin <- getPrfHasSinM tl'
      FGFO.Let <$@> el <*> cnv (eb , Ext tl' r) 
    _                                  -> fail "Type Error!" 