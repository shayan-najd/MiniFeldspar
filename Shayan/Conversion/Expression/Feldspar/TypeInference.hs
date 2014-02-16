{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
           , ScopedTypeVariables, GADTs, NoMonomorphismRestriction
           , ImplicitParams, ConstraintKinds #-}
module Conversion.Expression.Feldspar.TypeInference where

import qualified Expression.Feldspar.ADTUntypedDebruijn as FAUM
import qualified Expression.Feldspar.ADTChurch  as FACP
import qualified Type.Feldspar.ADTWithMetavariable as FAM

import qualified Environment.ADT         as A

import Conversion
import Conversion.Type.Feldspar ()
import Conversion.Variable ()
 
import TypeChecking.Feldspar.ADTChurch ()
import Unification.Feldspar.ADTWithMetavariable ()

import Inference
import InferenceMonad
import Data.Traversable(traverse)

instance (Cnv t FAM.Typ , Cnv FAM.Typ t') => 
          Cnv (FAUM.Exp , A.Env t) (FACP.Exp t') where
  cnv = \ (e , r) -> do r' :: A.Env FAM.Typ    <- cnv r
                        e' :: FACP.Exp FAM.Typ <- inf cnvExpUToACPAM (e , r')
                        traverse cnv e'
   where
     cnvExpUToACPAM eaum = case eaum of
       FAUM.ConI i       -> FACP.ConI <$> pure i
       FAUM.ConB b       -> FACP.ConB <$> pure b
       FAUM.Var v        -> FACP.Var  <$> pure v
       FAUM.Abs eb       -> FACP.Abs  <$> newMta <*@> eb
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
           ?cnv = cnvExpUToACPAM