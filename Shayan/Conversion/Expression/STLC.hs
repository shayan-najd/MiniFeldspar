{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
           , ScopedTypeVariables, GADTs, NoMonomorphismRestriction
           , ImplicitParams, ConstraintKinds #-}
module Conversion.Expression.STLC where

import qualified Expression.STLC.ADTUntypedPolymorphic  as SAUP
import qualified Expression.STLC.ADTUntypedMonomorphic  as SAUM
import qualified Expression.STLC.ADTChurchPolymorphic   as SACP
import qualified Expression.STLC.ADTExplicitPolymorphic as SAEP
import qualified Expression.STLC.GADT                   as SGDT
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

import TypeChecking.STLC.ADTChurchPolymorphic   ()
import TypeChecking.STLC.ADTExplicitPolymorphic ()
import Unification.STLC.ADTWithMetavariable ()

import Inference
import InferenceMonad
import Existential
import Singleton

import Data.Traversable (traverse)

type ExsExp = Exs2 SGDT.Exp (G.Env G.Typ) G.Typ
type ExsTyp = ExsSin G.Typ
type SigTyp = Sin G.Typ
type SigEnv = Sin (G.Env G.Typ)

---------------------------------------------------------------------------------
-- Conversion from SAUP.Exp
---------------------------------------------------------------------------------
instance (Eq x , SigEnv r , SigTyp t' , Cnv t SAM.Typ) =>  
         Cnv (SAUP.Exp x , AT.Env x t , AT.Env x SAUM.Exp) (SGDT.Exp r t') where
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
instance (SigEnv r , SigTyp t' , Cnv t SAM.Typ) => 
         Cnv (SAUM.Exp , A.Env t) (SGDT.Exp r t') where
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
instance (SigEnv r , SigTyp t' , Cnv t' SAM.Typ , t ~ t') => 
         Cnv (SACP.Exp t , A.Env t) (SGDT.Exp r t') where
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
    return (Exs2 (SGDT.Con i) r' G.Int)
  cnv (SACP.Var x     , r) = do 
    Exs2 x' r' t' <- cnv (x , r)
    return (Exs2 (SGDT.Var x') r' t')
  cnv (SACP.Abs ta eb , r) = do 
    ExsSin ta'                    :: ExsTyp <- cnv ta
    Exs2 eb' (ta'' `G.Ext` r') tb :: ExsExp <- cnv(eb , ta : r)
    Rfl <- eqlSin ta' ta''
    return (Exs2 (SGDT.Abs eb') r' (G.Arr ta' tb))
  cnv (SACP.App ef ea , r) = do 
    Exs2 ef' rf (G.Arr ta tb)     :: ExsExp <- cnv (ef , r)
    Exs2 ea' ra ta'               :: ExsExp <- cnv (ea , r)
    Rfl <- eqlSin rf ra
    Rfl <- eqlSin ta ta'
    return (Exs2 (SGDT.App ef' ea') rf tb)
  cnv (SACP.Add el er , r) = do 
    Exs2 el' rl G.Int             :: ExsExp <- cnv (el , r)
    Exs2 er' rr G.Int             :: ExsExp <- cnv (er , r)
    Rfl <- eqlSin rl rr
    return (Exs2 (SGDT.Add el' er') rl G.Int)

instance Cnv t t' => Cnv (SACP.Exp t) (SACP.Exp t') where
   cnv = traverse cnv
   
---------------------------------------------------------------------------------
-- Conversion from SAEP.Exp
---------------------------------------------------------------------------------
instance (SigEnv r , SigTyp t' , Cnv t' SAM.Typ , t ~ t') => 
         Cnv (SAEP.Exp t , A.Env t) (SGDT.Exp r t') where
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
    return (Exs2 (SGDT.Con i) r' G.Int)
  cnv (SAEP.Var _ x     , r) = do 
    Exs2 x' r' t' <- cnv (x , r)
    return (Exs2 (SGDT.Var x') r' t')
  cnv (SAEP.Abs t eb , r) = do 
    let SAM.Arr ta _ = t
    ExsSin ta'                    :: ExsTyp <- cnv ta
    Exs2 eb' (ta'' `G.Ext` r') tb :: ExsExp <- cnv (eb , ta : r)
    Rfl <- eqlSin ta' ta''
    return (Exs2 (SGDT.Abs eb') r' (G.Arr ta' tb))
  cnv (SAEP.App _ ef ea , r) = do 
    Exs2 ef' rf (G.Arr ta tb)     :: ExsExp <- cnv (ef , r)
    Exs2 ea' ra ta'               :: ExsExp <- cnv (ea , r)
    Rfl <- eqlSin rf ra
    Rfl <- eqlSin ta ta'
    return (Exs2 (SGDT.App ef' ea') rf tb)
  cnv (SAEP.Add _ el er , r) = do 
    Exs2 el' rl G.Int             :: ExsExp <- cnv (el , r)
    Exs2 er' rr G.Int             :: ExsExp <- cnv (er , r)
    Rfl <- eqlSin rl rr
    return (Exs2 (SGDT.Add el' er') rl G.Int)

instance Cnv t t' => Cnv (SAEP.Exp t) (SAEP.Exp t') where
   cnv = traverse cnv

---------------------------------------------------------------------------------
-- Conversion from Higher-Order
---------------------------------------------------------------------------------
instance (t ~ t' , r ~ r') => 
         Cnv (SGDT.Exp r t , G.Env G.Typ r) (SGHO.Exp r' t') where
  cnv = \ (e , r) -> return (cnvGToGHO e (cnvGEnv r))
    where
      cnvGToGHO :: forall rr tt. SGDT.Exp rr tt -> G.Env (SGHO.Exp rr) rr 
                   -> SGHO.Exp rr tt
      cnvGToGHO egdt r = case egdt of  
        SGDT.Con i     -> SGHO.Con i
        SGDT.Var v     -> G.gets v r
        SGDT.Add el er -> SGHO.Add (cnvGToGHO el r) (cnvGToGHO er r)
        SGDT.App ef ea -> SGHO.App (cnvGToGHO ef r) (cnvGToGHO ea r)
        SGDT.Abs eb    -> SGHO.Abs (\ x -> prdAll 
                                           (cnvGToGHO eb (wkn (G.Ext x r))))
 
wkn :: G.Env (SGHO.Exp r) r' -> G.Env (SGHO.Exp (t , r)) r'
wkn G.Emp                    = G.Emp
wkn (G.Ext e G.Emp)          = G.Ext (sucAll e) G.Emp
wkn (G.Ext e es@(G.Ext _ _)) = G.Ext (sucAll e) (wkn es)
 
sucAll :: SGHO.Exp r t' -> SGHO.Exp (t , r) t' 
sucAll (SGHO.Con i)      = SGHO.Con i
sucAll (SGHO.Var v)      = SGHO.Var (G.Suc v)
sucAll (SGHO.Add el er)  = SGHO.Add (sucAll el) (sucAll er)
sucAll (SGHO.App ef ea)  = SGHO.App (sucAll ef) (sucAll ea)
sucAll (SGHO.Abs f)      = SGHO.Abs (sucAll . f . prdAll)  

-- Should not contain variable zro
prdAll :: SGHO.Exp (t , r) t' -> SGHO.Exp r t'
prdAll (SGHO.Con i)         = SGHO.Con i
prdAll (SGHO.Var (G.Suc v)) = SGHO.Var v
prdAll (SGHO.Var G.Zro)     = error "Impossible!"
prdAll (SGHO.Add el er)     = SGHO.Add (prdAll el) (prdAll er)
prdAll (SGHO.App ef ea)     = SGHO.App (prdAll ef) (prdAll ea)
prdAll (SGHO.Abs f)         = SGHO.Abs (prdAll . f . sucAll) 
                                                  
cnvGEnv :: G.Env G.Typ r -> G.Env (SGHO.Exp r) r  
cnvGEnv  = undefined 

{-
-- Having the following

type R = (Integer,(Integer -> Integer,(Integer ,())))

t1 :: G.Env G.Typ R
t1 = G.Ext G.Int (G.Ext (G.Arr G.Int G.Int)(G.Int `G.Ext` G.Emp))

t2 :: G.Env (G.Var R) R
t2 = G.Ext G.Zro (G.Ext (G.Suc G.Zro)((G.Suc $ G.Suc G.Zro) `G.Ext` G.Emp))
 
-- We would like to have cnvGEnv t1 == t2
-}