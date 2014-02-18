{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
           , ScopedTypeVariables, GADTs, NoMonomorphismRestriction
           , ImplicitParams, ConstraintKinds #-}
module Conversion.Expression.STLC.Lifting where

import Prelude hiding (sin)
 
import qualified Expression.STLC.GADTFirstOrder         as SGFO
import qualified Expression.STLC.GADTHigherOrder        as SGHO

import qualified Singleton.TypeSTLC    as G
import qualified Singleton.Environment as G

import Conversion
import Conversion.Type.STLC ()
import Conversion.Variable ()
import Conversion.Existential ()

import TypeChecking.STLC.ADTChurch   ()
import TypeChecking.STLC.ADTExplicit ()
import Unification.STLC.ADTWithMetavariable ()
 
import Singleton
import Singleton.TypeSTLC ()

type SinTyp = HasSin G.Typ
type SinEnv = HasSin (G.Env G.Typ)

instance (t ~ t' , r ~ r' , SinEnv r) => 
         Cnv (SGFO.Exp r t) (SGHO.Exp r' t') where
  cnv e = return (cnvGToGHO e (cnvGEnv (sin :: G.Env G.Typ r )))
 
cnvGToGHO :: forall rr tt. SGFO.Exp rr tt -> G.Env (SGHO.Exp rr) rr 
             -> SGHO.Exp rr tt
cnvGToGHO egdt r = 
  let c ::  forall t. SGFO.Exp rr t -> SGHO.Exp rr t
      c e = cnvGToGHO e r in
  case egdt of  
    SGFO.Con i     -> SGHO.Con i
    SGFO.Var v     -> G.gets v r
    SGFO.Abs ta eb -> SGHO.Abs ta (\ x -> SGHO.prdAll 
                                          (cnvGToGHO eb 
                                           (G.wkn SGHO.sucAll (G.Ext x r))))
    SGFO.Add el er -> SGHO.Add (c el) (c er)
    SGFO.App ef ea -> SGHO.App (c ef) (c ea)

cnvGEnv :: G.Env G.Typ r -> G.Env (SGHO.Exp r) r  
cnvGEnv = G.map SGHO.Var . G.cnvGEnvtoGVar
