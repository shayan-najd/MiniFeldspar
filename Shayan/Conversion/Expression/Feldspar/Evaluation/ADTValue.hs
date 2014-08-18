module Conversion.Expression.Feldspar.Evaluation.ADTValue () where

import MyPrelude

import qualified Expression.Feldspar.ADTValue  as FAV
import qualified Expression.Feldspar.GADTValue as FGV
import qualified Type.Feldspar.GADT            as TFG

import Conversion
import Conversion.Variable ()
import Singleton

instance HasSin TFG.Typ t =>  Cnv (FAV.Exp , ()) (FGV.Exp t) where
  cnv (ee , r) = let ?r = r in case (ee , sin :: TFG.Typ t) of
    (FAV.ConI i        , TFG.Int)       -> FGV.conI <$@> i
    (FAV.ConB b        , TFG.Bol)       -> FGV.conB <$@> b
    (FAV.ConF f        , TFG.Flt)       -> FGV.conF <$@> f
    (FAV.Abs f         , TFG.Arr ta tb) -> do PrfHasSin <- getPrfHasSinM ta
                                              PrfHasSin <- getPrfHasSinM tb
                                              FGV.abs <$> pure
                                                (FGV.getTrm . samTyp tb
                                                 . frmRgt . cnvImp
                                                 . f
                                                 . frmRgt . cnvImp
                                                 . samTyp ta . FGV.Exp)
    (FAV.Tpl (vf , vs) , TFG.Tpl tf ts) -> do PrfHasSin <- getPrfHasSinM tf
                                              PrfHasSin <- getPrfHasSinM ts
                                              FGV.tpl <$@> vf <*@> vs
    (FAV.Ary v         , TFG.Ary ta)    -> do PrfHasSin <- getPrfHasSinM ta
                                              (fmap FGV.Exp
                                               . mapM (fmap FGV.getTrm
                                                       . samTypM ta
                                                       . cnvImp)) v
    (FAV.Cmx c         , TFG.Cmx)       -> pure (FGV.Exp c)--todo:make symmetric
    _                                   -> fail "Type Error!"

instance HasSin TFG.Typ t => Cnv (FGV.Exp t , ()) FAV.Exp where
  cnv (FGV.Exp ee , r) = let ?r = r in case sin :: TFG.Typ t of
    TFG.Int       -> FAV.ConI <$@> ee
    TFG.Bol       -> FAV.ConB <$@> ee
    TFG.Flt       -> FAV.ConF <$@> ee
    TFG.Arr ta tb -> do PrfHasSin <- getPrfHasSinM ta
                        PrfHasSin <- getPrfHasSinM tb
                        pure (FAV.Abs (frmRgt . cnvImp . samTyp tb
                                       . FGV.Exp . ee . FGV.getTrm
                                       . samTyp ta . frmRgt . cnvImp))
    TFG.Tpl tf ts -> do PrfHasSin <- getPrfHasSinM tf
                        PrfHasSin <- getPrfHasSinM ts
                        curry FAV.Tpl
                          <$@> (samTyp tf . FGV.Exp . MyPrelude.fst) ee
                          <*@> (samTyp ts . FGV.Exp . MyPrelude.snd) ee
    TFG.Ary ta    -> do PrfHasSin <- getPrfHasSinM ta
                        FAV.Ary <$@> (fmap (samTyp ta . FGV.Exp) ee)
    TFG.Cmx       -> FAV.Cmx <$@> ee