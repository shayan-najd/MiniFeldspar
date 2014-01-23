{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
           , ScopedTypeVariables, GADTs, NoMonomorphismRestriction
           , ImplicitParams, ConstraintKinds #-}
module Conversion.Expression.Feldspar where

import qualified Expression.Feldspar.ADTUntypedPolymorphic as FAUP
import qualified Expression.Feldspar.ADTUntypedMonomorphic as FAUM
import qualified Expression.Feldspar.ADTChurchPolymorphic  as FACP
import qualified Expression.Feldspar.GADT                  as FGDT
 
import qualified Type.Feldspar.ADTSimple           as FAS
import qualified Type.Feldspar.ADTWithMetavariable as FAM
import qualified Type.Feldspar.GADT                as FGD

import qualified Variable.ADT            as A

import qualified Environment.ADT         as A
import qualified Environment.ADTTable    as AT
import qualified Environment.GADT        as G

import Conversion
import Conversion.Type.Feldspar ()
import Conversion.Variable ()
import Conversion.Environment (cnvEnvAMAS)
import Conversion.Existential ()

import SingletonEquality
import SingletonEquality.EnvironmentGADT ()
import SingletonEquality.TypeFeldsparGADT ()

import TypeChecking.Feldspar.ADTChurchPolymorphic ()
import Unification.Feldspar.ADTWithMetavariable ()

import Inference
import InferenceMonad
import Existential
import Singleton

import Data.Traversable(traverse)

type ExsTyp = ExsSin FGD.Typ
type ExsExp = Exs2 FGDT.Exp (G.Env FGD.Typ) FGD.Typ
type SinTyp = Sin FGD.Typ
type SinEnv = Sin (G.Env FGD.Typ)
  
---------------------------------------------------------------------------------
-- Conversion from FAUP
---------------------------------------------------------------------------------
instance (Eq x , SinEnv r , SinTyp t' , Cnv t FAM.Typ) => 
         Cnv (FAUP.Exp x , AT.Env x t , AT.Env x FAUM.Exp) (FGDT.Exp r t')  where
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
 
instance (Eq x , Cnv t FAM.Typ) => 
         Cnv (FAUP.Exp x , AT.Env x t , AT.Env x FAUM.Exp) FAUM.Exp where
  cnv = \ (e , rt , rf) -> do rt' :: AT.Env x FAM.Typ <- cnvEnvAMAS rt 
                              cnvExpUUToU e (zip (map fst rt') [A.Zro ..]) rf
    where
      cnvExpUUToU eaup rb rf = case eaup of
        FAUP.ConI i       -> FAUM.ConI <$> pure i
        FAUP.ConB b       -> FAUM.ConB <$> pure b
        FAUP.Var s        -> case (AT.get s rb , AT.get s rf) of
          (Just x  , _)      -> FAUM.Var <$> pure x
          (Nothing , Just e) -> pure e
          _                  -> fail "Scope Error!"
        FAUP.Abs s  eb    -> FAUM.Abs <$> 
                             cnvExpUUToU eb ((s,A.Zro) : fmap succ `map` rb) rf 
        FAUP.Let s el eb  -> FAUM.Let <$@> el <*> 
                             cnvExpUUToU eb ((s,A.Zro) : fmap succ `map` rb) rf 
        FAUP.App ef ea    -> FAUM.App <$@> ef <*@> ea
        FAUP.Cnd ec et ef -> FAUM.Cnd <$@> ec <*@> et <*@> ef 
        FAUP.Whl ec eb ei -> FAUM.Whl <$@> ec <*@> eb <*@> ei 
        FAUP.Tpl ef es    -> FAUM.Tpl <$@> ef <*@> es 
        FAUP.Fst e        -> FAUM.Fst <$@> e 
        FAUP.Snd e        -> FAUM.Snd <$@> e 
        FAUP.Ary el ef    -> FAUM.Ary <$@> el <*@> ef 
        FAUP.Ind ea ei    -> FAUM.Ind <$@> ea <*@> ei 
        FAUP.Len e        -> FAUM.Len <$@> e
        where
          ?cnv = \ e -> cnvExpUUToU e rb rf   
                         
---------------------------------------------------------------------------------
-- Conversion from FAUM
---------------------------------------------------------------------------------
instance (SinEnv r , SinTyp t' , Cnv t FAM.Typ) => 
         Cnv (FAUM.Exp , A.Env t) (FGDT.Exp r t')  where
  cnv (e , r) = do e' :: ExsExp <- cnv (e , r)       
                   cnv e'

instance Cnv t FAM.Typ => 
         Cnv (FAUM.Exp, A.Env t) ExsExp where
  cnv (e , r) = do r' :: A.Env FAM.Typ    <- cnv r 
                   e' :: FACP.Exp FAM.Typ <- cnv (e , r')
                   cnv (e' , r')

instance (Cnv t FAM.Typ , Cnv FAM.Typ t') => 
          Cnv (FAUM.Exp , A.Env t) (FACP.Exp t') where
  cnv = \ (e , r) -> do r' :: A.Env FAM.Typ    <- cnv r
                        e' :: FACP.Exp FAM.Typ <- inf cnvExpUToACPAM (e , r')
                        cnv e'
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

---------------------------------------------------------------------------------
-- Conversion from FACP
---------------------------------------------------------------------------------
instance (SinEnv r , SinTyp t' , Cnv t' FAM.Typ, t ~ t') => 
         Cnv (FACP.Exp t, A.Env t) (FGDT.Exp r t')  where
  cnv (e , r) = do r'  :: A.Env    FAM.Typ <- cnv r
                   e'  :: FACP.Exp FAM.Typ <- cnv e
                   e'' :: ExsExp           <- cnv (e' , r')       
                   cnv e''

instance Cnv (FACP.Exp FAS.Typ , A.Env FAS.Typ) ExsExp where
  cnv (e , r) = do r' :: A.Env FAM.Typ    <- cnv r
                   e' :: FACP.Exp FAM.Typ <- cnv e
                   cnv (e' , r') 

instance Cnv (FACP.Exp FAM.Typ , A.Env FAM.Typ) ExsExp where
  cnv (FACP.ConI i , r)       = do 
    ExsSin r' <- cnv r
    return (Exs2 (FGDT.ConI i) r' FGD.Int)
  cnv (FACP.ConB i , r)       = do 
    ExsSin r' <- cnv r
    return (Exs2 (FGDT.ConB i) r' FGD.Bol)
  cnv (FACP.Var x  , r)       = do 
    Exs2 x' r' t' <- cnv (x , r)
    return (Exs2 (FGDT.Var x') r' t')
  cnv (FACP.Abs ta eb , r)    = do 
    ExsSin ta'                    :: ExsTyp <- cnv ta
    Exs2 eb' (ta'' `G.Ext` r') tb :: ExsExp <- cnv(eb , ta : r)
    Rfl <- eqlSin ta' ta''
    return (Exs2 (FGDT.Abs eb') r' (FGD.Arr ta' tb))
  cnv (FACP.App ef ea , r)    = do 
    Exs2 ef' rf (FGD.Arr ta tb) :: ExsExp <- cnv (ef , r)
    Exs2 ea' ra ta'             :: ExsExp <- cnv (ea , r)
    Rfl <- eqlSin rf ra
    Rfl <- eqlSin ta ta'
    return (Exs2 (FGDT.App ef' ea') rf tb)
  cnv (FACP.Cnd ec et ef , r) = do
    Exs2 ec' rc FGD.Bol :: ExsExp <- cnv (ec , r)
    Exs2 et' rt tt      :: ExsExp <- cnv (et , r)
    Exs2 ef' rf tf      :: ExsExp <- cnv (ef , r)
    Rfl <- eqlSin rc rt
    Rfl <- eqlSin rc rf
    Rfl <- eqlSin tt tf
    return (Exs2 (FGDT.Cnd ec' et' ef') rc tt)
  cnv (FACP.Whl ec eb ei , r) = do 
    Exs2 ec' rc (FGD.Arr tac FGD.Bol) :: ExsExp <- cnv (ec , r)
    Exs2 eb' rb (FGD.Arr tab tbb )    :: ExsExp <- cnv (eb , r)
    Exs2 ei' ri ti                    :: ExsExp <- cnv (ei , r)
    Rfl <- eqlSin rc rb
    Rfl <- eqlSin rc ri
    Rfl <- eqlSin tac tab
    Rfl <- eqlSin tac tbb
    Rfl <- eqlSin tac ti
    return (Exs2 (FGDT.Whl ec' eb' ei') rc tac)
  cnv (FACP.Tpl ef es , r)    = do 
    Exs2 ef' rf tf :: ExsExp <- cnv (ef , r)
    Exs2 es' rs ts :: ExsExp <- cnv (es , r)
    Rfl <- eqlSin rf rs
    Rfl <- eqlSin tf ts
    return (Exs2 (FGDT.Tpl ef' es') rf (FGD.Tpl tf ts))
  cnv (FACP.Fst e , r)        = do 
    Exs2 e' r' (FGD.Tpl tf _) :: ExsExp <- cnv (e , r)
    return (Exs2 (FGDT.Fst e') r' tf)
  cnv (FACP.Snd e , r)        = do 
    Exs2 e' r' (FGD.Tpl _ ts) :: ExsExp <- cnv (e , r)
    return (Exs2 (FGDT.Snd e') r' ts)  
  cnv (FACP.Ary el ef , r)    = do 
    Exs2 el' rl FGD.Int              :: ExsExp <- cnv (el , r)
    Exs2 ef' rf (FGD.Arr FGD.Int ta) :: ExsExp <- cnv (ef , r)
    Rfl <- eqlSin rl rf
    return (Exs2 (FGDT.Ary el' ef') rl (FGD.Ary ta))
  cnv (FACP.Len e , r)        = do 
    Exs2 e' r' (FGD.Ary _)  :: ExsExp <- cnv (e , r)
    return (Exs2 (FGDT.Len e') r' FGD.Int)
  cnv (FACP.Ind ea ei , r)    = do 
    Exs2 ea' ra (FGD.Ary ta) :: ExsExp <- cnv (ea , r)
    Exs2 ei' ri FGD.Int      :: ExsExp <- cnv (ei , r)
    Rfl <- eqlSin ra ri
    return (Exs2 (FGDT.Ind ea' ei') ra ta)
  cnv (FACP.Let el eb , r)    = do 
    Exs2 el' rl tl               :: ExsExp  <- cnv (el , r)
    tl'                          :: FAM.Typ <- cnv tl
    Exs2 eb' (G.Ext  tl'' rb) tb :: ExsExp  <- cnv (eb , tl' : r)
    Rfl <- eqlSin tl tl''
    Rfl <- eqlSin rb rl
    return (Exs2 (FGDT.Let el' eb') rb tb)

instance Cnv t t' => Cnv (FACP.Exp t) (FACP.Exp t') where
   cnv = traverse cnv