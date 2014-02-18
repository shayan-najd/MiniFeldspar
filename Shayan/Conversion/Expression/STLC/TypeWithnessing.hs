{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
           , ScopedTypeVariables, GADTs, NoMonomorphismRestriction
           , ImplicitParams, ConstraintKinds #-}
module Conversion.Expression.STLC.TypeWithnessing where

import Prelude hiding (sin)

import qualified Expression.STLC.ADTChurch   as SACP
import qualified Expression.STLC.ADTExplicit as SAEP
import qualified Expression.STLC.GADTFirstOrder         as SGFO
 
import qualified Type.STLC.ADTWithMetavariable as SAM
import qualified Singleton.TypeSTLC            as G
 
import qualified Environment.ADT          as A
import qualified Singleton.Environment    as G

import Conversion
import Conversion.Type.STLC ()
import Conversion.Variable ()
import Conversion.Existential ()

import SingletonEquality
import SingletonEquality.TypeSTLCGADT ()
import SingletonEquality.EnvironmentGADT ()

import TypeChecking.STLC.ADTChurch   ()
import TypeChecking.STLC.ADTExplicit ()
import Unification.STLC.ADTWithMetavariable ()

import Existential
import Singleton
import Singleton.TypeSTLC ()

type ExsExp = Exs2 SGFO.Exp (G.Env G.Typ) G.Typ
type ExsTyp = ExsSin G.Typ
type SinTyp = HasSin G.Typ
type SinEnv = HasSin (G.Env G.Typ)

instance Cnv (SACP.Exp SAM.Typ , A.Env SAM.Typ) ExsExp where  
  cnv (SACP.Con i     , r) = do 
    ExsSin r' <- cnv r
    return (Exs2 (SGFO.Con i) r' G.Int)
  cnv (SACP.Var x     , r) = do 
    Exs2 x' r' t' <- cnv (x , r)
    return (Exs2 (SGFO.Var x') r' t')
  cnv (SACP.Abs ta eb , r) = do 
    ExsSin ta'                    :: ExsTyp <- cnv ta
    Exs2 eb' (ta'' `G.Ext` r') tb :: ExsExp <- cnv(eb , ta : r)
    Rfl <- eqlSin ta' ta''
    return (Exs2 (SGFO.Abs ta' eb') r' (G.Arr ta' tb))
  cnv (SACP.App ef ea , r) = do 
    Exs2 ef' rf (G.Arr ta tb)     :: ExsExp <- cnv (ef , r)
    Exs2 ea' ra ta'               :: ExsExp <- cnv (ea , r)
    Rfl <- eqlSin rf ra
    Rfl <- eqlSin ta ta'
    return (Exs2 (SGFO.App ef' ea') rf tb)
  cnv (SACP.Add el er , r) = do 
    Exs2 el' rl G.Int             :: ExsExp <- cnv (el , r)
    Exs2 er' rr G.Int             :: ExsExp <- cnv (er , r)
    Rfl <- eqlSin rl rr
    return (Exs2 (SGFO.Add el' er') rl G.Int)

instance Cnv (SAEP.Exp SAM.Typ , A.Env SAM.Typ) ExsExp where  
  cnv (SAEP.Con _ i     , r) = do 
    ExsSin r' <- cnv r
    return (Exs2 (SGFO.Con i) r' G.Int)
  cnv (SAEP.Var _ x     , r) = do 
    Exs2 x' r' t' <- cnv (x , r)
    return (Exs2 (SGFO.Var x') r' t')
  cnv (SAEP.Abs t eb , r) = do 
    let SAM.Arr ta _ = t
    ExsSin ta'                    :: ExsTyp <- cnv ta
    Exs2 eb' (ta'' `G.Ext` r') tb :: ExsExp <- cnv (eb , ta : r)
    Rfl <- eqlSin ta' ta''
    return (Exs2 (SGFO.Abs ta' eb') r' (G.Arr ta' tb))
  cnv (SAEP.App _ ef ea , r) = do 
    Exs2 ef' rf (G.Arr ta tb)     :: ExsExp <- cnv (ef , r)
    Exs2 ea' ra ta'               :: ExsExp <- cnv (ea , r)
    Rfl <- eqlSin rf ra
    Rfl <- eqlSin ta ta'
    return (Exs2 (SGFO.App ef' ea') rf tb)
  cnv (SAEP.Add _ el er , r) = do 
    Exs2 el' rl G.Int             :: ExsExp <- cnv (el , r)
    Exs2 er' rr G.Int             :: ExsExp <- cnv (er , r)
    Rfl <- eqlSin rl rr
    return (Exs2 (SGFO.Add el' er') rl G.Int)
