module Conversion.Expression.Feldspar.TypeInference where

import qualified Expression.Feldspar.GADTUntypedDebruijn as FGUD
import qualified Expression.Feldspar.GADTTyped           as FGCP

import qualified Data.Vector   as A
import qualified Type.Herbrand as H
import qualified Type.Feldspar as FT

import Conversion
import Conversion.Type.Feldspar ()
import Conversion.Variable ()
 
import TypeChecking.Feldspar ()

import Inference
import Data.Traversable(traverse)

instance n ~ n' => Cnv (FGUD.Exp n , A.Vec n FT.Typ) (FGCP.Exp n' FT.Typ) where
  cnv (e , r) = do e' :: FGCP.Exp n () <- cnv e 
                   cnv (e' , r)                     

instance n ~ n' => Cnv (FGUD.Exp n) (FGCP.Exp n' ()) where
  cnv eaum = case eaum of
       FGUD.ConI i       -> FGCP.ConI <$> pure i
       FGUD.ConB b       -> FGCP.ConB <$> pure b
       FGUD.Var v        -> FGCP.Var  <$> pure v
       FGUD.Abs eb       -> FGCP.Abs  <$> cnv eb
       FGUD.App ef ea    -> FGCP.App  <$> pure () <*@> ef <*@> ea
       FGUD.Cnd ec et ef -> FGCP.Cnd  <$@> ec <*@> et <*@> ef 
       FGUD.Whl ec eb ei -> FGCP.Whl  <$> cnv ec <*> cnv eb <*@> ei
       FGUD.Tpl ef es    -> FGCP.Tpl  <$@> ef <*@> es
       FGUD.Fst e        -> FGCP.Fst  <$> pure () <*@> e
       FGUD.Snd e        -> FGCP.Snd  <$> pure () <*@> e
       FGUD.Ary el ef    -> FGCP.Ary  <$@> el <*> cnv ef
       FGUD.Len e        -> FGCP.Len  <$> pure () <*@> e 
       FGUD.Ind ea ei    -> FGCP.Ind  <$@> ea <*@> ei
       FGUD.Let el eb    -> FGCP.Let  <$> pure () <*@> el <*> cnv eb
       where
           ?cnv = cnv

instance n ~ n' => Cnv (FGCP.Exp n () , A.Vec n FT.Typ)(FGCP.Exp n' FT.Typ) where
  cnv (e , r) = do r' :: A.Vec n (H.Typ (H.EnvFld '[])) <- cnv r
                   e' <- typInf e r'
                   traverse cnv e'
 