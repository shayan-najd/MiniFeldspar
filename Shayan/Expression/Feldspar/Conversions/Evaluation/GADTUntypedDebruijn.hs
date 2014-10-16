module Expression.Feldspar.Conversions.Evaluation.GADTUntypedDebruijn () where

import MyPrelude

import Expression.Feldspar.GADTUntypedDebruijn
import qualified Expression.Feldspar.ADTValue as FAV

import Environment.Scoped

import Nat.ADT

import Conversion
import Variable.Conversion ()

instance Cnv (Exp n , Env n FAV.Exp) FAV.Exp where
  cnv (ee , r) = let ?r = r in join (case ee of
    ConI i       -> FAV.conI <$@> i
    ConB b       -> FAV.conB <$@> b
    ConF b       -> FAV.conF <$@> b
    Var x        -> FAV.var  <$@> x
    Abs eb       -> FAV.abs  <$@> eb
    App ef ea    -> FAV.app  <$@> ef <*@> ea
    Cnd ec et ef -> FAV.cnd  <$@> ec <*@> et <*@> ef
    Whl ec eb ei -> FAV.whl  <$@> ec <*@> eb <*@> ei
    Tpl ef es    -> FAV.tpl  <$@> ef <*@> es
    Fst e        -> FAV.fst  <$@> e
    Snd e        -> FAV.snd  <$@> e
    Ary el ef    -> FAV.ary  <$@> el <*@> ef
    Len e        -> FAV.len  <$@> e
    Ind ea ei    -> FAV.ind  <$@> ea <*@> ei
    Let el eb    -> return   <$@> App (Abs eb) el
    Cmx er ei    -> FAV.cmx  <$@> er <*@> ei
    Non          -> pure FAV.non
    Som e        -> FAV.som  <$@> e
    May em en es -> FAV.may  <$@> em <*@> en <*@> es)

instance Cnv (Exp (Suc n) , Env n FAV.Exp) (FAV.Exp -> FAV.Exp) where
  cnv (e , r) = pure (frmRgt . curry cnv e . flip Ext r)

{- Todo!
instance n ~ n' =>
         Cnv (FAV.Exp , Env n' FAV.Exp)  (Exp n) where
  cnv (v , r) = let ?r = r in case v of
    FAV.ConI i        -> ConI <$@> i
    FAV.ConB b        -> ConB <$@> b
--    FAV.Abs  f        ->
    FAV.Tpl (vf , vs) -> Tpl  <$@> vf <*@> vs
    FAV.Arr av
      | fst (bounds av) == 0 -> do f <- cnv (FAV.Abs (fromJust
                                                 . flip lookup (assocs av)
                                                 . (\ (FAV.ConI i) -> i))
                                            , Ext _ r )

                                   Ary <$@> (FAV.ConI . snd . bounds) av
                                       <*>  pure f
      | otherwise            -> fail "Bad Array!"


--instance Cnv (FAV.Exp -> FAV.Exp , Env n FAV.Exp) (Exp (Suc n)) where
--  cnv (e , r) = pure (f  cnv)

-}