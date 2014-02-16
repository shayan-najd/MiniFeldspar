{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
           , ScopedTypeVariables, GADTs, NoMonomorphismRestriction
           , ImplicitParams, ConstraintKinds #-}
module Conversion.Expression.STLC.TypeInference where

import qualified Expression.STLC.ADTUntypedDebruijn  as SAUM
import qualified Expression.STLC.ADTChurch   as SACP
import qualified Expression.STLC.ADTExplicit as SAEP
 
import qualified Type.STLC.ADTWithMetavariable as SAM

import qualified Environment.ADT          as A
 
import Conversion
import Conversion.Type.STLC ()
import Conversion.Variable ()
import Conversion.Existential ()
 
import TypeChecking.STLC.ADTChurch   ()
import TypeChecking.STLC.ADTExplicit ()
import Unification.STLC.ADTWithMetavariable ()

import Inference
import InferenceMonad
 
import Data.Traversable (traverse)

instance (Cnv t SAM.Typ , Cnv SAM.Typ t') => 
         Cnv (SAUM.Exp , A.Env t) (SACP.Exp t') where
  cnv = \ (e , r) -> do r' :: A.Env SAM.Typ    <- cnv r
                        e' :: SACP.Exp SAM.Typ <- inf cnvExpUToACPAM (e , r')
                        traverse cnv e'
   where
     cnvExpUToACPAM eaum = case eaum of
       SAUM.Con i     -> SACP.Con <$> pure i
       SAUM.Var v     -> SACP.Var <$> pure v
       SAUM.Abs eb    -> SACP.Abs <$> newMta <*@> eb
       SAUM.App ef ea -> SACP.App <$@> ef <*@> ea
       SAUM.Add el er -> SACP.Add <$@> el <*@> er 
       where
           ?cnv = cnvExpUToACPAM

instance (Cnv t SAM.Typ , Cnv SAM.Typ t') => 
         Cnv (SAUM.Exp , A.Env t) (SAEP.Exp t') where
  cnv = \ (e , r) -> do r' :: A.Env SAM.Typ    <- cnv r
                        e' :: SAEP.Exp SAM.Typ <- inf cnvExpUToAEPAM (e , r')
                        traverse cnv e'
   where
    cnvExpUToAEPAM eaum = case eaum of
       SAUM.Con i     -> SAEP.Con <$> newMta <*> pure i
       SAUM.Var v     -> SAEP.Var <$> newMta <*> pure v
       SAUM.Abs eb    -> SAEP.Abs <$> newMta <*@> eb
       SAUM.App ef ea -> SAEP.App <$> newMta <*@> ef <*@> ea
       SAUM.Add el er -> SAEP.Add <$> newMta <*@> el <*@> er 
       where
         ?cnv = cnvExpUToAEPAM
