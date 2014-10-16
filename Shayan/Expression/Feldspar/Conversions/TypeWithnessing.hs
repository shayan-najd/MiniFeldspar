module Expression.Feldspar.Conversions.TypeWithnessing () where

import MyPrelude

import qualified Expression.Feldspar.GADTTyped      as FGTD
import qualified Expression.Feldspar.GADTFirstOrder as FGFO

import qualified Type.Feldspar.GADT                 as TFG
import qualified Type.Feldspar.ADT                  as TFA

import Environment.Typed

import Conversion
import Variable.Conversion      ()

import Singleton

type ExsTyp = ExsSin TFG.Typ

instance (r ~ r' , n ~ Len r , HasSin TFG.Typ t) =>
         Cnv (FGTD.Exp n TFA.Typ , Env TFG.Typ r)
             (FGFO.Exp r' t) where
  cnv (ee , r) = let ?r = r in let t = sin :: TFG.Typ t in case (ee , t) of
    (FGTD.ConI i       , TFG.Int)      -> pure (FGFO.ConI i)
    (FGTD.ConB b       , TFG.Bol)      -> pure (FGFO.ConB b)
    (FGTD.ConF f       , TFG.Flt)      -> pure (FGFO.ConF f)
    (FGTD.Var x        , _)            -> FGFO.Var  <$@> x
    (FGTD.Abs eb       , TFG.Arr ta _) -> case TFG.getPrfHasSinArr t of
      (PrfHasSin , PrfHasSin)          -> FGFO.Abs  <$@> (ta , eb)
    (FGTD.App ta ef ea , _)            -> do ExsSin ta' :: ExsTyp <- cnv ta
                                             PrfHasSin <- getPrfHasSinM ta'
                                             ea' <- cnvImp ea
                                             FGFO.App <$@> ef
                                                       <*> pure (samTyp ta' ea')
    (FGTD.Cnd ec et ef , _)            -> FGFO.Cnd <$@> ec <*@> et <*@> ef
    (FGTD.Whl ec eb ei , _)            -> FGFO.Whl <$@> (t , ec) <*@> (t , eb)
                                          <*@> ei
    (FGTD.Tpl ef es    , TFG.Tpl _ _)  -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin)          -> FGFO.Tpl <$@> ef <*@> es
    (FGTD.Fst ts e     , _)            -> do ExsSin ts' <- cnv ts
                                             PrfHasSin  <- getPrfHasSinM ts'
                                             e'         <- cnvImp e
                                             FGFO.Fst <$> pure
                                                      (samTyp (TFG.Tpl t ts') e')
    (FGTD.Snd tf e     , _)            -> do ExsSin tf' <- cnv tf
                                             PrfHasSin  <- getPrfHasSinM tf'
                                             e'         <- cnvImp e
                                             FGFO.Snd <$> pure
                                                      (samTyp (TFG.Tpl tf' t) e')
    (FGTD.Ary el ef    , TFG.Ary _)    -> case TFG.getPrfHasSinAry t of
      PrfHasSin                        -> FGFO.Ary <$@> el <*@> (TFG.Int , ef)
    (FGTD.Len ta e     , TFG.Int )     -> do ExsSin ta' :: ExsTyp <- cnv ta
                                             PrfHasSin <- getPrfHasSinM ta'
                                             e' <- cnvImp e
                                             FGFO.Len <$> pure
                                                      (samTyp (TFG.Ary ta') e')
    (FGTD.Ind e  ei    , _)            -> FGFO.Ind <$@> e  <*@> ei
    (FGTD.Cmx er ei    , TFG.Cmx)      -> FGFO.Cmx <$@> er <*@> ei
    (FGTD.Let tl el eb , _)            -> do ExsSin tl' :: ExsTyp <- cnv tl
                                             PrfHasSin <- getPrfHasSinM tl'
                                             FGFO.Let <$@> el <*@> (tl' , eb)
    (FGTD.Non          , TFG.May _)    -> case TFG.getPrfHasSinMay t of
      PrfHasSin                        -> pure FGFO.Non
    (FGTD.Som e        , TFG.May _)    -> case TFG.getPrfHasSinMay t of
      PrfHasSin                        -> FGFO.Som <$@> e
    (FGTD.May tm em en es,_        )   -> do ExsSin t' :: ExsTyp <- cnv tm
                                             PrfHasSin <- getPrfHasSinM t'
                                             em' <- cnvImp em
                                             FGFO.May
                                               <$> pure (samTyp (TFG.May t') em')
                                               <*@> en <*@> (t' , es)
    _                                  -> fail "Type Error!"

instance (r ~ r' , n ~ Len (tr ': r) , HasSin TFG.Typ t , tr ~ tr') =>
         Cnv ((TFG.Typ tr , FGTD.Exp n TFA.Typ) , Env TFG.Typ r)
             (FGFO.Exp (tr' ': r') t) where
  cnv ((t , ee) , r) = cnv (ee , Ext t r)

instance (n ~ Len r , HasSin TFG.Typ t) =>
         Cnv (FGFO.Exp r t , Env TFG.Typ r) (FGTD.Exp n TFA.Typ)
              where
  cnv (ee , r) = let ?r = r in let t = sin :: TFG.Typ t in case ee of
    FGFO.ConI i               -> FGTD.ConI <$@> i
    FGFO.ConB b               -> FGTD.ConB <$@> b
    FGFO.ConF f               -> FGTD.ConF <$@> f
    FGFO.Var x                -> FGTD.Var  <$@> x
    FGFO.Abs eb               -> case TFG.getPrfHasSinArr t of
      (PrfHasSin , PrfHasSin) -> FGTD.Abs <$@> eb
    FGFO.App ef ea            -> FGTD.App <$@> sinTypOf ea t <*@> ef <*@> ea
    FGFO.Cnd ec et ef         -> FGTD.Cnd <$@> ec <*@> et <*@> ef
    FGFO.Whl ec eb ei         -> FGTD.Whl <$@> ec <*@> eb <*@> ei
    FGFO.Tpl ef es            -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin) -> FGTD.Tpl <$@> ef <*@> es
    FGFO.Fst e                -> FGTD.Fst <$@> TFG.getSndTyp(sinTyp e) <*@> e
    FGFO.Snd e                -> FGTD.Snd <$@> TFG.getFstTyp(sinTyp e) <*@> e
    FGFO.Ary el ef            -> case TFG.getPrfHasSinAry t of
      PrfHasSin               -> FGTD.Ary <$@> el <*@> ef
    FGFO.Len e                -> FGTD.Len <$@> TFG.getAryTyp(sinTyp e) <*@> e
    FGFO.Ind e  ei            -> FGTD.Ind <$@> e <*@> ei
    FGFO.Let el eb            -> FGTD.Let <$@> sinTypOf el t
                                          <*@> el <*@> eb
    FGFO.Cmx er  ei           -> FGTD.Cmx <$@> er <*@> ei
    FGFO.Non                  -> pure FGTD.Non
    FGFO.Som e                -> case TFG.getPrfHasSinMay t of
      PrfHasSin               -> FGTD.Som <$@> e
    FGFO.May em en es         -> FGTD.May <$@> TFG.getMayTyp(sinTyp em)
                                 <*@> em <*@> en <*@> es

instance (n ~ Len (ta ': r) , HasSin TFG.Typ t, HasSin TFG.Typ ta) =>
         Cnv (FGFO.Exp (ta ': r) t , Env TFG.Typ r)
             (FGTD.Exp n TFA.Typ)
         where
  cnv (ee , r) = cnv (ee , Ext (sin :: TFG.Typ ta) r)