{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
           , ScopedTypeVariables, GADTs, NoMonomorphismRestriction
           , ImplicitParams, ConstraintKinds, PolyKinds #-}
module Conversion.Expression.STLC where

import Prelude hiding (sin)

import qualified Expression.STLC.ADTUntypedNamed  as SAUP
import qualified Expression.STLC.ADTUntypedDebruijn  as SAUM
import qualified Expression.STLC.ADTChurch   as SACP
import qualified Expression.STLC.ADTExplicit as SAEP
import qualified Expression.STLC.GADTFirstOrder         as SGFO
import qualified Expression.STLC.GADTHigherOrder        as SGHO

import qualified Type.STLC.ADTSimple           as SAS
import qualified Type.STLC.ADTWithMetavariable as SAM
import qualified Singleton.TypeSTLC            as G
 
import qualified Environment.ADT          as A
import qualified Environment.ADTTable     as AT
import qualified Singleton.Environment    as G

import Conversion
import Conversion.Type.STLC ()
import Conversion.Variable ()
import Conversion.Environment (cnvEnvAMAS)
import Conversion.Existential ()
import Conversion.Expression.STLC.NameResolution ()
import Conversion.Expression.STLC.Lifting ()
import Conversion.Expression.STLC.TypeWithnessing ()
import Conversion.Expression.STLC.TypeInference ()

import SingletonEquality.TypeSTLCGADT ()
import SingletonEquality.EnvironmentGADT ()

import TypeChecking.STLC.ADTChurch   ()
import TypeChecking.STLC.ADTExplicit ()
import Unification.STLC.ADTWithMetavariable ()

import Existential
import Singleton
import Singleton.TypeSTLC ()

import Data.Traversable (traverse)

type ExsExp = Exs2 SGFO.Exp (G.Env G.Typ) G.Typ
type ExsTyp = ExsSin G.Typ
type SinTyp = HasSin G.Typ
type SinEnv = HasSin (G.Env G.Typ)

---------------------------------------------------------------------------------
-- Conversion from SAUP.Exp
---------------------------------------------------------------------------------
instance (Eq x , SinEnv r , SinTyp t' , Cnv t SAM.Typ) => 
         Cnv (SAUP.Exp x , AT.Env x t , AT.Env x SAUM.Exp) (SGHO.Exp r t') where
  cnv (e , rt , rf) = do e' :: SGFO.Exp r t' <- cnv (e , rt , rf)         
                         cnv e'

instance (Eq x , SinEnv r , SinTyp t' , Cnv t SAM.Typ) =>  
         Cnv (SAUP.Exp x , AT.Env x t , AT.Env x SAUM.Exp) (SGFO.Exp r t') where
  cnv (e , rt , rf) = do e' :: SAUM.Exp <- cnv (e , rt , rf)
                         cnv (e', map snd rt)
                                          
instance (Eq x , Cnv t SAM.Typ) => 
         Cnv (SAUP.Exp x , AT.Env x t , AT.Env x SAUM.Exp) ExsExp  where
  cnv (e , rt , rf) = do e' :: SACP.Exp SAM.Typ <- cnv (e  , rt , rf)
                         r' :: AT.Env x SAM.Typ <- cnvEnvAMAS rt
                         cnv (e' , map snd r')                    

instance (Eq x , Cnv t SAM.Typ , Cnv SAM.Typ t') => 
         Cnv (SAUP.Exp x , AT.Env x t , AT.Env x SAUM.Exp) (SACP.Exp t') where
  cnv (e , rt , rf) = do r' :: AT.Env x SAM.Typ <- cnvEnvAMAS rt  
                         e' :: SAUM.Exp         <- cnv (e , r' , rf)
                         cnv (e'  , map snd r')

instance (Eq x , Cnv t SAM.Typ , Cnv SAM.Typ t') => 
         Cnv (SAUP.Exp x , AT.Env x t , AT.Env x SAUM.Exp) (SAEP.Exp t') where
  cnv (e , rt , rf) = do r' :: AT.Env x SAM.Typ <- cnvEnvAMAS rt  
                         e' :: SAUM.Exp         <- cnv (e , r' , rf)
                         cnv (e'  , map snd r')
---------------------------------------------------------------------------------
-- Conversion from SAUM.Exp
---------------------------------------------------------------------------------
instance (SinEnv r , SinTyp t' , Cnv t SAM.Typ) => 
         Cnv (SAUM.Exp , A.Env t) (SGHO.Exp r t') where
  cnv (e , r) = do e' :: SGFO.Exp r t' <- cnv (e , r)         
                   cnv e'

instance (SinEnv r , SinTyp t' , Cnv t SAM.Typ) => 
         Cnv (SAUM.Exp , A.Env t) (SGFO.Exp r t') where
  cnv (e , r) = do e' :: ExsExp <- cnv (e , r)
                   cnv e'               
    
instance Cnv t SAM.Typ =>  
         Cnv (SAUM.Exp , A.Env t) ExsExp where  
  cnv (e , r) = do r' :: A.Env SAM.Typ    <- cnv r
                   e' :: SACP.Exp SAM.Typ <- cnv (e , r')
                   cnv (e' , r')
---------------------------------------------------------------------------------
-- Conversion from SACP.Exp
---------------------------------------------------------------------------------
instance (SinEnv r , SinTyp t' , Cnv t SAM.Typ) => 
         Cnv (SACP.Exp t , A.Env t) (SGHO.Exp r t') where
  cnv (e , r) = do e' :: SGFO.Exp r t' <- cnv (e , r)         
                   cnv e'

instance (SinEnv r , SinTyp t' , Cnv t SAM.Typ) => 
         Cnv (SACP.Exp t , A.Env t) (SGFO.Exp r t') where
  cnv (e , r) = do r'  :: A.Env    SAM.Typ <- cnv r
                   e'  :: SACP.Exp SAM.Typ <- cnv e
                   e'' :: ExsExp           <- cnv (e' , r')       
                   cnv e''

instance Cnv (SACP.Exp SAS.Typ , A.Env SAS.Typ) ExsExp where
  cnv (e , r) = do r' :: A.Env SAM.Typ    <- cnv r
                   e' :: SACP.Exp SAM.Typ <- cnv e
                   cnv (e' , r') 
 
instance Cnv t t' => Cnv (SACP.Exp t) (SACP.Exp t') where
   cnv = traverse cnv   
---------------------------------------------------------------------------------
-- Conversion from SAEP.Exp
---------------------------------------------------------------------------------
instance (SinEnv r , SinTyp t' , Cnv t SAM.Typ) => 
         Cnv (SAEP.Exp t , A.Env t) (SGHO.Exp r t') where
  cnv (e , r) = do e' :: SGFO.Exp r t' <- cnv (e , r)         
                   cnv e'

instance (SinEnv r , SinTyp t' , Cnv t SAM.Typ) => 
         Cnv (SAEP.Exp t , A.Env t) (SGFO.Exp r t') where
  cnv (e , r) = do r'  :: A.Env    SAM.Typ <- cnv r
                   e'  :: SAEP.Exp SAM.Typ <- cnv e
                   e'' :: ExsExp           <- cnv (e' , r')       
                   cnv e''

instance Cnv (SAEP.Exp SAS.Typ , A.Env SAS.Typ) ExsExp where
  cnv (e , r) = do r' :: A.Env SAM.Typ    <- cnv r
                   e' :: SAEP.Exp SAM.Typ <- cnv e
                   cnv (e' , r') 

instance Cnv t t' => Cnv (SAEP.Exp t) (SAEP.Exp t') where
   cnv = traverse cnv