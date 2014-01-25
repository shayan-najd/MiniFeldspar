{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
           , ScopedTypeVariables, GADTs, NoMonomorphismRestriction
           , ImplicitParams, ConstraintKinds #-}
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
import qualified Type.STLC.GADT                as G

import qualified Variable.ADT             as A
import qualified Variable.GADT            as G

import qualified Environment.ADT          as A
import qualified Environment.ADTTable     as AT
import qualified Environment.GADT         as G

import Conversion
import Conversion.Type.STLC ()
import Conversion.Variable ()
import Conversion.Environment (cnvEnvAMAS)
import Conversion.Existential ()

import SingletonEquality
import SingletonEquality.TypeSTLCGADT ()
import SingletonEquality.EnvironmentGADT ()

import TypeChecking.STLC.ADTChurch   ()
import TypeChecking.STLC.ADTExplicit ()
import Unification.STLC.ADTWithMetavariable ()

import Inference
import InferenceMonad
import Existential
import Singleton
import Singleton.TypeSTLC ()

import Data.Traversable (traverse)

type ExsExp = Exs2 SGFO.Exp (G.Env G.Typ) G.Typ
type ExsTyp = ExsSin G.Typ
type SinTyp = Sin G.Typ
type SinEnv = Sin (G.Env G.Typ)

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
 
instance (Eq x , Cnv t SAM.Typ) => 
         Cnv (SAUP.Exp x , AT.Env x t , AT.Env x SAUM.Exp) SAUM.Exp where
  cnv = \ (e , rt , rf) -> do rt' :: AT.Env x SAM.Typ <- cnvEnvAMAS rt 
                              cnvExpUUToU e (zip (map fst rt') [A.Zro ..]) rf
    where
      cnvExpUUToU eaup rb rf = case eaup of      
        SAUP.Con i     -> return (SAUM.Con i)
        SAUP.Var s     -> case (AT.get s rb , AT.get s rf) of
          (Just x  , _)      -> SAUM.Var <$> pure x
          (Nothing , Just e) -> pure e
          _                  -> fail "Scope Error!"
        SAUP.Abs s  eb -> SAUM.Abs <$> 
                          cnvExpUUToU eb ((s , A.Zro) : fmap A.Suc `map` rb) rf 
        SAUP.App ef ea -> SAUM.App <$@> ef <*@> ea
        SAUP.Add el er -> SAUM.Add <$@> el <*@> er
        where
          ?cnv = \e -> cnvExpUUToU e rb rf 
 
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

instance (Cnv t SAM.Typ , Cnv SAM.Typ t') => 
         Cnv (SAUM.Exp , A.Env t) (SACP.Exp t') where
  cnv = \ (e , r) -> do r' :: A.Env SAM.Typ    <- cnv r
                        e' :: SACP.Exp SAM.Typ <- inf cnvExpUToACPAM (e , r')
                        cnv e'
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
                        cnv e'
   where
    cnvExpUToAEPAM eaum = case eaum of
       SAUM.Con i     -> SAEP.Con <$> newMta <*> pure i
       SAUM.Var v     -> SAEP.Var <$> newMta <*> pure v
       SAUM.Abs eb    -> SAEP.Abs <$> newMta <*@> eb
       SAUM.App ef ea -> SAEP.App <$> newMta <*@> ef <*@> ea
       SAUM.Add el er -> SAEP.Add <$> newMta <*@> el <*@> er 
       where
         ?cnv = cnvExpUToAEPAM

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

instance Cnv t t' => Cnv (SAEP.Exp t) (SAEP.Exp t') where
   cnv = traverse cnv

---------------------------------------------------------------------------------
-- Conversion to Higher-Order
---------------------------------------------------------------------------------
instance (t ~ t' , r ~ r' , SinEnv r) => 
         Cnv (SGFO.Exp r t) (SGHO.Exp r' t') where
  cnv e = do r :: G.Env G.Typ r <- sin 
             return (cnvGToGHO e (cnvGEnv r))
 
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
cnvGEnv = mapGEnv . G.cnvGEnvtoGVar
 

mapGEnv :: G.Env (G.Var r) r' -> G.Env (SGHO.Exp r) r'
mapGEnv G.Emp        = G.Emp
mapGEnv (G.Ext x xs) = G.Ext (SGHO.Var x) (mapGEnv xs) 

