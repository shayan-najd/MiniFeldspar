module Expression.Feldspar.Conversions.Evaluation.ADTValue () where

import MyPrelude

import qualified Expression.Feldspar.ADTValue  as FAV
import qualified Expression.Feldspar.GADTValue as FGV
import qualified Type.Feldspar.GADT            as TFG

import Conversion
import Variable.Conversion ()
import Singleton

instance HasSin TFG.Typ t =>  Cnv (FAV.Exp , ()) (FGV.Exp t) where
  cnv (ee , r) = let ?r = r in let t = sin :: TFG.Typ t in case (ee , t) of
    (FAV.ConI i        , TFG.Int)       -> pure (FGV.conI i)
    (FAV.ConB b        , TFG.Bol)       -> pure (FGV.conB b)
    (FAV.ConF f        , TFG.Flt)       -> pure (FGV.conF f)
    (FAV.Abs f         , TFG.Arr ta tb) -> case TFG.getPrfHasSinArr t of
       (PrfHasSin , PrfHasSin)          -> FGV.abs <$> pure
                                           (FGV.getTrm . samTyp tb
                                                   . frmRgt . cnvImp
                                                   . f
                                                   . frmRgt . cnvImp
                                                   . samTyp ta . FGV.Exp)
    (FAV.Tpl (vf , vs) , TFG.Tpl _ _)   -> case TFG.getPrfHasSinTpl t of
       (PrfHasSin , PrfHasSin)          -> FGV.tpl <$@> vf <*@> vs
    (FAV.Ary v         , TFG.Ary ta)    -> case TFG.getPrfHasSinAry t of
       PrfHasSin                        -> (fmap FGV.Exp
                                            . mapM (fmap FGV.getTrm
                                                    . samTypM ta
                                                    . cnvImp)) v
    (FAV.Cmx c         , TFG.Cmx)       -> pure (FGV.Exp c)
    (FAV.Non           , TFG.May _)     -> pure FGV.non
    (FAV.Som v         , TFG.May _)     -> case TFG.getPrfHasSinMay t of
       PrfHasSin                        -> FGV.som <$@> v
    _                                   -> fail "Type Error!"

instance HasSin TFG.Typ t => Cnv (FGV.Exp t , ()) FAV.Exp where
  cnv (FGV.Exp ee , r) = let ?r = r in let t = sin :: TFG.Typ t in case t of
    TFG.Int                   -> pure (FAV.ConI ee)
    TFG.Bol                   -> pure (FAV.ConB ee)
    TFG.Flt                   -> pure (FAV.ConF ee)
    TFG.Arr ta tb             -> case TFG.getPrfHasSinArr t of
      (PrfHasSin , PrfHasSin) -> pure (FAV.Abs (frmRgt . cnvImp . samTyp tb
                                       . FGV.Exp . ee . FGV.getTrm
                                       . samTyp ta . frmRgt . cnvImp))
    TFG.Tpl tf ts             -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin) -> curry FAV.Tpl
                                 <$@> (samTyp tf . FGV.Exp . MyPrelude.fst) ee
                                 <*@> (samTyp ts . FGV.Exp . MyPrelude.snd) ee
    TFG.Ary ta                -> case TFG.getPrfHasSinAry t of
       PrfHasSin              -> FAV.Ary <$@> (fmap (samTyp ta . FGV.Exp) ee)
    TFG.Cmx                   -> pure (FAV.Cmx ee)
    TFG.May ta                -> case TFG.getPrfHasSinMay t of
       PrfHasSin              -> case ee of
         Nothing -> pure FAV.Non
         Just e  -> FAV.Som <$@> (samTyp ta (FGV.Exp e))