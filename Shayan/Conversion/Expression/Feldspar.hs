{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
           , ScopedTypeVariables, GADTs, NoMonomorphismRestriction
           , ImplicitParams, ConstraintKinds, PolyKinds #-}
module Conversion.Expression.Feldspar where

import Prelude hiding (sin)
import qualified Expression.Feldspar.ADTUntypedNamed as FAUP
import qualified Expression.Feldspar.ADTUntypedDebruijn as FAUM
import qualified Expression.Feldspar.ADTChurch  as FACP
import qualified Expression.Feldspar.GADTFirstOrder        as FGFO

import qualified Type.Feldspar.ADTSimple           as FAS
import qualified Type.Feldspar.ADTWithMetavariable as FAM
import qualified Singleton.TypeFeldspar            as FG

import qualified Environment.ADT         as A
import qualified Environment.ADTTable    as AT
import qualified Singleton.Environment   as G

import Conversion
import Conversion.Type.Feldspar ()
import Conversion.Variable ()
import Conversion.Environment (cnvEnvAMAS)
import Conversion.Existential ()
import Conversion.Expression.Feldspar.NameResolution ()
import Conversion.Expression.Feldspar.TypeInference ()
import Conversion.Expression.Feldspar.TypeWithnessing ()
import Conversion.Expression.Feldspar.Lifting ()

import SingletonEquality.EnvironmentGADT ()
import SingletonEquality.TypeFeldsparGADT ()

import TypeChecking.Feldspar.ADTChurch ()
import Unification.Feldspar.ADTWithMetavariable ()

import Existential
import Singleton

import Data.Traversable(traverse)

type ExsTyp = ExsSin FG.Typ
type ExsExp = Exs2 FGFO.Exp (G.Env FG.Typ) FG.Typ
type SinTyp = HasSin FG.Typ
type SinEnv = HasSin (G.Env FG.Typ)
  
---------------------------------------------------------------------------------
-- Conversion from FAUP
---------------------------------------------------------------------------------
instance (Eq x , SinEnv r , SinTyp t' , Cnv t FAM.Typ) => 
         Cnv (FAUP.Exp x , AT.Env x t , AT.Env x FAUM.Exp) (FGFO.Exp r t')  where
  cnv (e , rt , rf) = do e' :: FAUM.Exp <- cnv (e , rt , rf)
                         cnv (e', map snd rt)

instance (Eq x , Cnv t FAM.Typ) => 
         Cnv (FAUP.Exp x , AT.Env x t , AT.Env x FAUM.Exp) ExsExp  where
  cnv (e , rt , rf) = do e' :: FACP.Exp FAM.Typ <- cnv (e  , rt , rf)
                         r' :: AT.Env x FAM.Typ <- cnvEnvAMAS rt
                         cnv (e' , map snd r')                   

instance (Eq x , Cnv t FAM.Typ , Cnv FAM.Typ t') => 
         Cnv (FAUP.Exp x , AT.Env x t , AT.Env x FAUM.Exp) (FACP.Exp t') where
  cnv (e , rt , rf) = do r' :: AT.Env x FAM.Typ <- cnvEnvAMAS rt  
                         e' :: FAUM.Exp         <- cnv (e , r' , rf)
                         cnv (e'  , map snd r')                          
---------------------------------------------------------------------------------
-- Conversion from FAUM
---------------------------------------------------------------------------------
instance (SinEnv r , SinTyp t' , Cnv t FAM.Typ) => 
         Cnv (FAUM.Exp , A.Env t) (FGFO.Exp r t')  where
  cnv (e , r) = do e' :: ExsExp <- cnv (e , r)       
                   cnv e'

instance Cnv t FAM.Typ => 
         Cnv (FAUM.Exp, A.Env t) ExsExp where
  cnv (e , r) = do r' :: A.Env FAM.Typ    <- cnv r 
                   e' :: FACP.Exp FAM.Typ <- cnv (e , r')
                   cnv (e' , r')
---------------------------------------------------------------------------------
-- Conversion from FACP
---------------------------------------------------------------------------------
instance (SinEnv r , SinTyp t' , Cnv t FAM.Typ) => 
         Cnv (FACP.Exp t, A.Env t) (FGFO.Exp r t')  where
  cnv (e , r) = do r'  :: A.Env    FAM.Typ <- cnv r
                   e'  :: FACP.Exp FAM.Typ <- cnv e
                   e'' :: ExsExp           <- cnv (e' , r')       
                   cnv e''

instance Cnv (FACP.Exp FAS.Typ , A.Env FAS.Typ) ExsExp where
  cnv (e , r) = do r' :: A.Env FAM.Typ    <- cnv r
                   e' :: FACP.Exp FAM.Typ <- cnv e
                   cnv (e' , r') 

instance Cnv t t' => Cnv (FACP.Exp t) (FACP.Exp t') where
   cnv = traverse cnv   
 