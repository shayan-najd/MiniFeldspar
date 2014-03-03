module Conversion.Expression.Feldspar.TypeInference where

import qualified Expression.Feldspar.ADTUntypedDebruijn as FAUM
import qualified Expression.Feldspar.ADTChurch  as FACP

import qualified Environment.ADT as A
import qualified Type.Herbrand   as H
import qualified Type.Feldspar   as FT

import Conversion
import Conversion.Type.Feldspar ()
import Conversion.Variable ()
 
import TypeChecking.Feldspar.ADTChurch ()

import Inference
import Data.Traversable(traverse)

instance Cnv (FAUM.Exp , A.Env FT.Typ) (FACP.Exp FT.Typ) where
  cnv (e , r) = do e' :: FACP.Exp () <- cnv e 
                   cnv (e' , r)                     

instance Cnv FAUM.Exp (FACP.Exp ()) where
  cnv eaum = case eaum of
       FAUM.ConI i       -> FACP.ConI <$> pure i
       FAUM.ConB b       -> FACP.ConB <$> pure b
       FAUM.Var v        -> FACP.Var  <$> pure v
       FAUM.Abs eb       -> FACP.Abs  <$> pure () <*@> eb
       FAUM.App ef ea    -> FACP.App  <$@> ef <*@> ea
       FAUM.Cnd ec et ef -> FACP.Cnd  <$@> ec <*@> et <*@> ef 
       FAUM.Whl ec eb ei -> FACP.Whl  <$@> ec <*@> eb <*@> ei
       FAUM.Tpl ef es    -> FACP.Tpl  <$@> ef <*@> es
       FAUM.Ary el ef    -> FACP.Ary  <$@> el <*@> ef
       FAUM.Ind ea ei    -> FACP.Ind  <$@> ea <*@> ei
       FAUM.Fst e        -> FACP.Fst  <$@> e
       FAUM.Snd e        -> FACP.Snd  <$@> e
       FAUM.Len e        -> FACP.Len  <$@> e 
       FAUM.Let el eb    -> FACP.Let  <$@> el <*@> eb
       where
           ?cnv = cnv

instance Cnv (FACP.Exp () , A.Env FT.Typ) (FACP.Exp FT.Typ) where
  cnv (e , r) = do r' :: A.Env (H.Typ (H.EnvFld '[])) <- cnv r
                   e' <- typInf e r'
                   traverse cnv e'
 