{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GADTs, ConstraintKinds
           , ScopedTypeVariables , FlexibleContexts, PolyKinds, TypeHoles #-}
module Conversion.Expression.STLC where

import Prelude hiding (sin)
import qualified Expression.STLC.ADTUntypedPolymorphic  as AUP
import qualified Expression.STLC.ADTUntypedMonomorphic  as AUM
import qualified Expression.STLC.ADTChurchPolymorphic   as ACP
import qualified Expression.STLC.ADTExplicitPolymorphic as AEP
import qualified Expression.STLC.GADT                   as G
import qualified Expression.STLC.GADTHigherOrder        as GHO

import qualified Type.STLC.ADTSimple           as AS
import qualified Type.STLC.ADTWithMetavariable as AM
import qualified Type.STLC.GADT                as G

import qualified Variable.ADT             as A
import qualified Variable.GADT            as G

import qualified Environment.ADTTable     as AT
import qualified Environment.ADT          as A
import qualified Environment.GADT         as G

import TypeChecking.STLC.ADTChurchPolymorphic   ()
import TypeChecking.STLC.ADTExplicitPolymorphic ()

import InferenceMonad
import Unification.STLC.ADTWithMetavariable ()

import SingletonEquality.TypeSTLCGADT ()
import SingletonEquality.EnvironmentGADT ()

import Conversion.Type.STLC ()
import Conversion.Variable ()
import Conversion.Environment ()
import Conversion.Existential ()

import Existential
import Singleton
import Conversion
import SingletonEquality
import ErrorMonad
import Inference
import qualified Conversion.Type.STLC as C

import Control.Applicative ((<$>),(<*>),pure)
import Data.Traversable (traverse)

type ExsExp = Exs2 G.Exp (G.Env G.Typ) G.Typ
type ExsTyp = ExsSin G.Typ
type SigTyp = Sin G.Typ
type SigEnv = Sin (G.Env G.Typ)
---------------------------------------------------------------------------------
-- Conversion from AUP.Exp
---------------------------------------------------------------------------------
instance (SigTyp t , SigEnv r , Eq x) => 
                 Cnv (AUP.Exp x , AT.Env x AS.Typ) (G.Exp r t)      where
  cnv (e , r) = do e' :: ExsExp <- cnv (e , r)           
                   cnv e'
                     
instance (SigTyp t , SigEnv r , Eq x) =>  
         Cnv (AUP.Exp x , AT.Env x AS.Typ , AT.Env x AUM.Exp) (G.Exp r t) where
  cnv (e , rt , rs) = do e' :: AUM.Exp <- cnv (e , rt , rs)
                         cnv (e', map snd rt)
                                          
instance Eq a => Cnv (AUP.Exp a , AT.Env a AS.Typ) ExsExp            where
  cnv (e , rt) = do e' :: AUM.Exp <- cnv (e , rt)
                    cnv (e' , map snd rt)                 

instance Eq a => Cnv (AUP.Exp a , AT.Env a AS.Typ) (ACP.Exp AM.Typ) where
  cnv (e , rt) = do e' :: AUM.Exp <- cnv (e , rt)
                    cnv (e' , map snd rt)                 

instance Eq a => Cnv (AUP.Exp a , AT.Env a AM.Typ) (ACP.Exp AS.Typ) where  
  cnv (e , rt) = do e' :: AUM.Exp <- cnv (e , rt)
                    cnv (e' , map snd rt)                 
  
instance Eq a => Cnv (AUP.Exp a , AT.Env a AM.Typ) (ACP.Exp AM.Typ) where  
  cnv (e , rt) = do e' :: AUM.Exp <- cnv (e , rt)
                    cnv (e' , map snd rt)                 

instance Eq a => Cnv (AUP.Exp a , AT.Env a AS.Typ) (ACP.Exp AS.Typ) where  
  cnv (e , rt) = do e' :: AUM.Exp <- cnv (e , rt)
                    cnv (e' , map snd rt)                 

instance Eq a => Cnv (AUP.Exp a , AT.Env a AS.Typ) (AEP.Exp AM.Typ) where
  cnv (e , rt) = do e' :: AUM.Exp <- cnv (e , rt)
                    cnv (e' , map snd rt)                 
  
instance Eq a => Cnv (AUP.Exp a , AT.Env a AM.Typ) (AEP.Exp AS.Typ) where  
  cnv (e , rt) = do e' :: AUM.Exp <- cnv (e , rt)
                    cnv (e' , map snd rt)                 
  
instance Eq a => Cnv (AUP.Exp a , AT.Env a AM.Typ) (AEP.Exp AM.Typ) where  
  cnv (e , rt) = do e' :: AUM.Exp <- cnv (e , rt)
                    cnv (e' , map snd rt)                 
  
instance Eq a => Cnv (AUP.Exp a , AT.Env a AS.Typ) (AEP.Exp AS.Typ) where  
  cnv (e , rt) = do e' :: AUM.Exp <- cnv (e , rt)
                    cnv (e' , map snd rt)                 
  
instance Eq a => Cnv (AUP.Exp a , AT.Env a AM.Typ) AUM.Exp          where
  cnv (e , rt) = cnvExpUUToU e (zip (map fst rt) [A.Zro ..]) []  

instance Eq a => Cnv (AUP.Exp a , AT.Env a AS.Typ) AUM.Exp          where
  cnv (e , rt) = cnv (e , rt , [] :: AT.Env a AUM.Exp)  

instance Eq a => 
         Cnv (AUP.Exp a , AT.Env a AS.Typ , AT.Env a AUM.Exp) AUM.Exp where
  cnv (e , rt , rf) = cnvExpUUToU e (zip (map fst rt) [A.Zro ..]) rf 

cnvExpUUToU :: Eq a => 
               AUP.Exp a -> AT.Env a A.Nat -> AT.Env a AUM.Exp -> ErrM AUM.Exp
cnvExpUUToU (AUP.Con i)     _  _  = return (AUM.Con i)
cnvExpUUToU (AUP.Var s)     rb rf = case (AT.get s rb , AT.get s rf) of
  (Just x  , _)                  -> return (AUM.Var x)
  (Nothing , Just e)             -> return e
  _                              -> fail "Scope Error!"
cnvExpUUToU (AUP.Abs s  eb) rb rf = AUM.Abs <$> 
                                    cnvExpUUToU eb ((s , A.Zro) : 
                                                    fmap A.Suc `map` rb) rf 
cnvExpUUToU (AUP.App ef ea) rb rf = AUM.App <$>
                                    cnvExpUUToU ef rb rf <*>
                                    cnvExpUUToU ea rb rf 
cnvExpUUToU (AUP.Add el er) rb rf = AUM.Add <$> 
                                    cnvExpUUToU el rb rf <*> 
                                    cnvExpUUToU er rb rf   

---------------------------------------------------------------------------------
-- Conversion from AUM.Exp
---------------------------------------------------------------------------------
instance (SigTyp t , SigEnv e) => 
         Cnv (AUM.Exp , A.Env AS.Typ) (G.Exp e t) where
  cnv (e , r) = do e' :: ExsExp <- cnv (e , r)
                   cnv e'               
    
instance Cnv (AUM.Exp , A.Env AS.Typ) ExsExp where  
  cnv (e , r) = do e' :: ACP.Exp AS.Typ <- cnv (e , r)
                   cnv (e' , r)

instance Cnv (AUM.Exp , A.Env AS.Typ) (ACP.Exp AM.Typ) where
  cnv (e , r) = do r' :: A.Env AM.Typ <- traverse cnv r
                   cnv (e , r')

instance Cnv (AUM.Exp , A.Env AM.Typ) (ACP.Exp AS.Typ) where
  cnv (e , r) = do e' :: ACP.Exp AM.Typ <- cnv (e , r)
                   traverse cnv e'

instance Cnv (AUM.Exp , A.Env AM.Typ) (ACP.Exp AM.Typ) where
  cnv = inf cnvExpUToACPAM   

instance Cnv (AUM.Exp , A.Env AS.Typ) (ACP.Exp AS.Typ) where
  cnv (e , r) = do r' :: A.Env AM.Typ   <- traverse cnv r
                   e' :: ACP.Exp AM.Typ <- cnv (e , r')
                   traverse cnv e'

instance Cnv (AUM.Exp , A.Env AS.Typ) (AEP.Exp AM.Typ) where
  cnv (e , r) = do r' :: A.Env AM.Typ <- traverse cnv r
                   cnv (e , r')

instance Cnv (AUM.Exp , A.Env AM.Typ) (AEP.Exp AS.Typ) where
  cnv (e , r) = do e' :: AEP.Exp AM.Typ <- cnv (e , r)
                   traverse cnv e'

instance Cnv (AUM.Exp , A.Env AM.Typ) (AEP.Exp AM.Typ) where
  cnv = inf cnvExpUToEAM

instance Cnv (AUM.Exp , A.Env AS.Typ) (AEP.Exp AS.Typ) where
  cnv (e , r) = do r' :: A.Env AM.Typ   <- traverse cnv r
                   e' :: AEP.Exp AM.Typ <- cnv (e , r')
                   traverse cnv e'
  
-- Conversion from the untyped lambda calculus to the explicitly typed 
-- simply-typed lambda calculus (church style) where every explicit type 
-- annotaion is set as a fresh metavariable
cnvExpUToACPAM :: AUM.Exp -> InfM C.EnvAMH (ACP.Exp AM.Typ)
cnvExpUToACPAM (AUM.Con i)     = return (ACP.Con i)
cnvExpUToACPAM (AUM.Var v)     = return (ACP.Var v)
cnvExpUToACPAM (AUM.Abs eb)    = ACP.Abs <$> newMta <*> cnvExpUToACPAM eb
cnvExpUToACPAM (AUM.App ef ea) = ACP.App <$> cnvExpUToACPAM ef <*> 
                                 cnvExpUToACPAM ea
cnvExpUToACPAM (AUM.Add ef ea) = ACP.Add <$> cnvExpUToACPAM ef <*> 
                                 cnvExpUToACPAM ea         

       
-- Conversion from the untyped lambda calculus to the explicitly typed 
-- simply-typed lambda calculus where every explicit type annotaion is
-- set as a fresh metavariable
cnvExpUToEAM :: AUM.Exp -> InfM C.EnvAMH (AEP.Exp AM.Typ)
cnvExpUToEAM (AUM.Con i)     = AEP.Con <$> newMta <*> pure i
cnvExpUToEAM (AUM.Var v)     = AEP.Var <$> newMta <*> pure v
cnvExpUToEAM (AUM.Abs eb)    = AEP.Abs <$> newMta <*> cnvExpUToEAM eb
cnvExpUToEAM (AUM.App ef ea) = AEP.App <$> newMta <*> cnvExpUToEAM ef <*> 
                               cnvExpUToEAM ea
cnvExpUToEAM (AUM.Add ef ea) = AEP.Add <$> newMta <*> cnvExpUToEAM ef <*> 
                               cnvExpUToEAM ea
---------------------------------------------------------------------------------
-- Conversion from ACP.Exp
---------------------------------------------------------------------------------
instance (SigTyp t , SigEnv e) => 
         Cnv (ACP.Exp AS.Typ , A.Env AS.Typ) (G.Exp e t) where
  cnv (e , r) = do e' :: ExsExp <- cnv (e , r)           
                   cnv e'  

instance Cnv (ACP.Exp AS.Typ , A.Env AS.Typ) ExsExp where  
  cnv (ACP.Con i     , r) = do ExsSin r' <- cnv r
                               return (Exs2 (G.Con i) r' G.Int)
  cnv (ACP.Var x     , r) = do Exs2 x' r' t' <- cnv (x , r)
                               return (Exs2 (G.Var x') r' t')
  cnv (ACP.Abs ta eb , r) = do ExsSin ta'   :: ExsTyp <- cnv ta
                               Exs2 eb' (ta'' `G.Ext` r') tb 
                                            :: ExsExp <- cnv(eb , ta : r)
                               Rfl <- eqlSin ta' ta''
                               return (Exs2 (G.Abs eb') r' (G.Arr ta' tb))
  cnv (ACP.App ef ea , r) = do Exs2 ef' rf (G.Arr ta tb) 
                                            :: ExsExp <- cnv (ef , r)
                               Exs2 ea' ra ta'           
                                            :: ExsExp <- cnv (ea , r)
                               Rfl <- eqlSin rf ra
                               Rfl <- eqlSin ta ta'
                               return (Exs2 (G.App ef' ea') rf tb)
  cnv (ACP.Add el er , r) = do Exs2 el' rl G.Int 
                                            :: ExsExp <- cnv (el , r)
                               Exs2 er' rr G.Int 
                                            :: ExsExp <- cnv (er , r)
                               Rfl <- eqlSin rl rr
                               return (Exs2 (G.Add el' er') rl G.Int)

---------------------------------------------------------------------------------
-- Conversion from AEP.Exp
---------------------------------------------------------------------------------
instance (SigTyp t , SigEnv e) => 
         Cnv (AEP.Exp AS.Typ , A.Env AS.Typ) (G.Exp e t) where
  cnv (e , r) = do e' :: ExsExp <- cnv (e , r)           
                   cnv e'  

instance Cnv (AEP.Exp AS.Typ , A.Env AS.Typ) ExsExp where
  cnv (AEP.Con _ i     , r) = do ExsSin r' <- cnv r
                                 return (Exs2 (G.Con i) r' G.Int)
  cnv (AEP.Var _ x     , r) = do Exs2 x' r' t' <- cnv (x , r)
                                 return (Exs2 (G.Var x') r' t')
                              
  cnv (AEP.Abs t eb    , r) = do let (ta `AS.Arr` _) = t
                                 ExsSin ta' :: ExsSin G.Typ <- cnv ta
                                 Exs2 eb' (ta'' `G.Ext` r') tb 
                                            :: ExsExp       <- cnv (eb , ta : r)
                                 Rfl <- eqlSin ta' ta''
                                 return (Exs2 (G.Abs eb') r' (G.Arr ta' tb))
     
  cnv (AEP.App _ ef ea , r) = do Exs2 ef' rf (G.Arr ta tb) 
                                            :: ExsExp <- cnv (ef , r)
                                 Exs2 ea' ra ta'           
                                            :: ExsExp <- cnv (ea , r)
                                 Rfl <- eqlSin rf ra
                                 Rfl <- eqlSin ta ta'
                                 return (Exs2 (G.App ef' ea') rf tb)
  cnv (AEP.Add _ el er , r) = do Exs2 el' rl G.Int 
                                            :: ExsExp <- cnv (el , r)
                                 Exs2 er' rr G.Int 
                                            :: ExsExp <- cnv (er , r)
                                 Rfl <- eqlSin rl rr
                                 return (Exs2 (G.Add el' er') rl G.Int)

---------------------------------------------------------------------------------
-- Conversion from Existentials
---------------------------------------------------------------------------------
instance t ~ t' => Cnv (G.Exp r t , G.Env G.Typ r) (GHO.Exp r t') where
  cnv (e , r) = cnv (e , cnvGEnv r)           
  
instance t ~ t' => Cnv (G.Exp r t , G.Env (GHO.Exp r) r) (GHO.Exp r t') where
  cnv  = return . cnvGToGHO

cnvGToGHO :: forall r t. (G.Exp r t , G.Env (GHO.Exp r) r) -> GHO.Exp r t
cnvGToGHO (G.Con i     , _) = GHO.Con i
cnvGToGHO (G.Var v     , r) = G.gets v r
cnvGToGHO (G.Add el er , r) = GHO.Add (cnvGToGHO (el , r)) (cnvGToGHO (er , r))
cnvGToGHO (G.App ef ea , r) = GHO.App (cnvGToGHO (ef , r)) (cnvGToGHO (ea , r))
cnvGToGHO (G.Abs eb    , r) = GHO.Abs (\ x -> prdAll 
                                              (cnvGToGHO (eb , wkn (G.Ext x r))))

wkn :: G.Env (GHO.Exp r) r' -> G.Env (GHO.Exp (t , r)) r'
wkn G.Emp                    = G.Emp
wkn (G.Ext e G.Emp)          = G.Ext (sucAll e) G.Emp
wkn (G.Ext e es@(G.Ext _ _)) = G.Ext (sucAll e) (wkn es)
 
sucAll :: GHO.Exp r t' -> GHO.Exp (t , r) t' 
sucAll (GHO.Con i)      = GHO.Con i
sucAll (GHO.Var v)      = GHO.Var (G.Suc v)
sucAll (GHO.Add el er)  = GHO.Add (sucAll el) (sucAll er)
sucAll (GHO.App ef ea)  = GHO.App (sucAll ef) (sucAll ea)
sucAll (GHO.Abs f)      = GHO.Abs (sucAll . f . prdAll)  

prdAll :: GHO.Exp (t , r) t' -> GHO.Exp r t'
prdAll (GHO.Con i)         = GHO.Con i
prdAll (GHO.Var (G.Suc v)) = GHO.Var v
prdAll (GHO.Var G.Zro)     = error "Impossible!"
prdAll (GHO.Add el er)     = GHO.Add (prdAll el) (prdAll er)
prdAll (GHO.App ef ea)     = GHO.App (prdAll ef) (prdAll ea)
prdAll (GHO.Abs f)         = GHO.Abs (prdAll . f . sucAll) 
                                                  
cnvGEnv :: G.Env G.Typ r -> G.Env (GHO.Exp r) r  
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