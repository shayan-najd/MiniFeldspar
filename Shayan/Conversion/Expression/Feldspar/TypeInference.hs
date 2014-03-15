module Conversion.Expression.Feldspar.TypeInference () where

import Prelude ()
import MyPrelude

import qualified Expression.Feldspar.GADTUntypedDebruijn as FGUD
import qualified Expression.Feldspar.GADTTyped           as FGTD

import qualified Type.Herbrand                           as TH
import qualified Type.Feldspar.ADT                       as TFA

import Environment.Scoped

import Conversion
import Conversion.Type.Feldspar ()
import Conversion.Variable      ()
 
import Inference                (typInf)
import TypeChecking.Feldspar    ()

 
instance n ~ n' => Cnv (FGUD.Exp n , Env n TFA.Typ) (FGTD.Exp n' TFA.Typ) where
  cnv (e , r) = do e' :: FGTD.Exp n () <- cnv (e , ()) 
                   cnv (e' , r)                     

instance n ~ n' => Cnv (FGUD.Exp n , ()) (FGTD.Exp n' ()) where
  cnv (eaum , ()) = let ?r = () in case eaum of
    FGUD.ConI i       -> FGTD.ConI <$@> i
    FGUD.ConB b       -> FGTD.ConB <$@> b
    FGUD.Var v        -> FGTD.Var  <$@> v
    FGUD.Abs eb       -> FGTD.Abs  <$@> eb
    FGUD.App ef ea    -> FGTD.App  <$@> () <*@> ef <*@> ea
    FGUD.Cnd ec et ef -> FGTD.Cnd  <$@> ec <*@> et <*@> ef 
    FGUD.Whl ec eb ei -> FGTD.Whl  <$@> ec <*@> eb <*@> ei
    FGUD.Tpl ef es    -> FGTD.Tpl  <$@> ef <*@> es
    FGUD.Fst e        -> FGTD.Fst  <$@> () <*@> e
    FGUD.Snd e        -> FGTD.Snd  <$@> () <*@> e
    FGUD.Ary el ef    -> FGTD.Ary  <$@> el <*@> ef
    FGUD.Len e        -> FGTD.Len  <$@> () <*@> e 
    FGUD.Ind ea ei    -> FGTD.Ind  <$@> ea <*@> ei
    FGUD.Let el eb    -> FGTD.Let  <$@> () <*@> el <*@> eb
 
instance n ~ n' => Cnv (FGTD.Exp n () , Env n TFA.Typ)(FGTD.Exp n' TFA.Typ) where
  cnv (e , r) = do r' :: Env n (TH.Typ (TH.EnvFld '[])) <- cnv (r , ())
                   e' <- typInf e r'
                   traverse (flip (curry cnv) ()) e'
 