{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
           , ScopedTypeVariables, GADTs, NoMonomorphismRestriction
           , ImplicitParams, ConstraintKinds #-}
module Conversion.Expression.Feldspar where

import Prelude hiding (sin)
import qualified Expression.Feldspar.ADTUntypedNamed as FAUP
import qualified Expression.Feldspar.ADTUntypedDebruijn as FAUM
import qualified Expression.Feldspar.ADTChurch  as FACP
import qualified Expression.Feldspar.GADTFirstOrder        as FGFO
import qualified Expression.Feldspar.GADTHigherOrder        as FGHO 

import qualified Type.Feldspar.ADTSimple           as FAS
import qualified Type.Feldspar.ADTWithMetavariable as FAM
import qualified Singleton.TypeFeldspar            as FG

import qualified Variable.ADT            as A
import qualified Variable.GADT           as G


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

import TypeChecking.Feldspar.ADTChurch ()
import Unification.Feldspar.ADTWithMetavariable ()

import Inference
import InferenceMonad
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
         Cnv (FAUM.Exp , A.Env t) (FGFO.Exp r t')  where
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

instance Cnv (FACP.Exp FAM.Typ , A.Env FAM.Typ) ExsExp where
  cnv (FACP.ConI i , r)       = do 
    ExsSin r' <- cnv r
    return (Exs2 (FGFO.ConI i) r' FG.Int)
  cnv (FACP.ConB i , r)       = do 
    ExsSin r' <- cnv r
    return (Exs2 (FGFO.ConB i) r' FG.Bol)
  cnv (FACP.Var x  , r)       = do 
    Exs2 x' r' t' <- cnv (x , r)
    return (Exs2 (FGFO.Var x') r' t')
  cnv (FACP.Abs ta eb , r)    = do 
    ExsSin ta'                    :: ExsTyp <- cnv ta
    Exs2 eb' (ta'' `G.Ext` r') tb :: ExsExp <- cnv(eb , ta : r)
    Rfl <- eqlSin ta' ta''
    return (Exs2 (FGFO.Abs ta' eb') r' (FG.Arr ta' tb))
  cnv (FACP.App ef ea , r)    = do 
    Exs2 ef' rf (FG.Arr ta tb) :: ExsExp <- cnv (ef , r)
    Exs2 ea' ra ta'             :: ExsExp <- cnv (ea , r)
    Rfl <- eqlSin rf ra
    Rfl <- eqlSin ta ta'
    return (Exs2 (FGFO.App ef' ea') rf tb)
  cnv (FACP.Cnd ec et ef , r) = do
    Exs2 ec' rc FG.Bol :: ExsExp <- cnv (ec , r)
    Exs2 et' rt tt      :: ExsExp <- cnv (et , r)
    Exs2 ef' rf tf      :: ExsExp <- cnv (ef , r)
    Rfl <- eqlSin rc rt
    Rfl <- eqlSin rc rf
    Rfl <- eqlSin tt tf
    return (Exs2 (FGFO.Cnd ec' et' ef') rc tt)
  cnv (FACP.Whl ec eb ei , r) = do 
    Exs2 ec' rc (FG.Arr tac FG.Bol) :: ExsExp <- cnv (ec , r)
    Exs2 eb' rb (FG.Arr tab tbb )    :: ExsExp <- cnv (eb , r)
    Exs2 ei' ri ti                    :: ExsExp <- cnv (ei , r)
    Rfl <- eqlSin rc rb
    Rfl <- eqlSin rc ri
    Rfl <- eqlSin tac tab
    Rfl <- eqlSin tac tbb
    Rfl <- eqlSin tac ti
    return (Exs2 (FGFO.Whl ec' eb' ei') rc tac)
  cnv (FACP.Tpl ef es , r)    = do 
    Exs2 ef' rf tf :: ExsExp <- cnv (ef , r)
    Exs2 es' rs ts :: ExsExp <- cnv (es , r)
    Rfl <- eqlSin rf rs
    Rfl <- eqlSin tf ts
    return (Exs2 (FGFO.Tpl ef' es') rf (FG.Tpl tf ts))
  cnv (FACP.Fst e , r)        = do 
    Exs2 e' r' (FG.Tpl tf _) :: ExsExp <- cnv (e , r)
    return (Exs2 (FGFO.Fst e') r' tf)
  cnv (FACP.Snd e , r)        = do 
    Exs2 e' r' (FG.Tpl _ ts) :: ExsExp <- cnv (e , r)
    return (Exs2 (FGFO.Snd e') r' ts)  
  cnv (FACP.Ary el ef , r)    = do 
    Exs2 el' rl FG.Int              :: ExsExp <- cnv (el , r)
    Exs2 ef' rf (FG.Arr FG.Int ta) :: ExsExp <- cnv (ef , r)
    Rfl <- eqlSin rl rf
    return (Exs2 (FGFO.Ary el' ef') rl (FG.Ary ta))
  cnv (FACP.Len e , r)        = do 
    Exs2 e' r' (FG.Ary _)  :: ExsExp <- cnv (e , r)
    return (Exs2 (FGFO.Len e') r' FG.Int)
  cnv (FACP.Ind ea ei , r)    = do 
    Exs2 ea' ra (FG.Ary ta) :: ExsExp <- cnv (ea , r)
    Exs2 ei' ri FG.Int      :: ExsExp <- cnv (ei , r)
    Rfl <- eqlSin ra ri
    return (Exs2 (FGFO.Ind ea' ei') ra ta)
  cnv (FACP.Let el eb , r)    = do 
    Exs2 el' rl tl               :: ExsExp  <- cnv (el , r)
    tl'                          :: FAM.Typ <- cnv tl
    Exs2 eb' (G.Ext  tl'' rb) tb :: ExsExp  <- cnv (eb , tl' : r)
    Rfl <- eqlSin tl tl''
    Rfl <- eqlSin rb rl
    return (Exs2 (FGFO.Let tl'' el' eb') rb tb)

instance Cnv t t' => Cnv (FACP.Exp t) (FACP.Exp t') where
   cnv = traverse cnv   

---------------------------------------------------------------------------------
-- Conversion to Higher-Order
---------------------------------------------------------------------------------
instance (t ~ t' , r ~ r' , SinEnv r) => 
         Cnv (FGFO.Exp r t) (FGHO.Exp r' t') where
  cnv e = return (cnvGToGHO e (cnvGEnv (sin :: G.Env FG.Typ r)))
 
cnvGToGHO :: forall rr tt. FGFO.Exp rr tt -> G.Env (FGHO.Exp rr) rr 
             -> (FGHO.Exp rr tt)
cnvGToGHO egdt r = 
  let c ::  forall t. FGFO.Exp rr t -> FGHO.Exp rr t
      c e = cnvGToGHO e r in
  case egdt of  
       FGFO.ConI i       -> FGHO.ConI i
       FGFO.ConB b       -> FGHO.ConB b
       FGFO.Var v        -> G.gets v r
       FGFO.Abs ta eb    -> FGHO.Abs ta (\ x -> FGHO.prdAll 
                                              (cnvGToGHO eb 
                                               (G.wkn FGHO.sucAll (G.Ext x r))))
       
       FGFO.App ef ea    -> FGHO.App (c ef) (c ea)
       FGFO.Cnd ec et ef -> FGHO.Cnd (c ec) (c et) (c ef) 
       FGFO.Whl ec eb ei -> FGHO.Whl (c ec) (c eb) (c ei)

       FGFO.Tpl ef es    -> FGHO.Tpl (c ef) (c es)
       FGFO.Ary el ef    -> FGHO.Ary (c el) (c ef)
       FGFO.Ind ea ei    -> FGHO.Ind (c ea) (c ei)
       FGFO.Fst e        -> FGHO.Fst (c e)
       FGFO.Snd e        -> FGHO.Snd (c e)
       FGFO.Len e        -> FGHO.Len (c e) 
       FGFO.Let tl el eb -> FGHO.Let tl (c el) 
                            (\ x -> FGHO.prdAll 
                                    (cnvGToGHO eb 
                                     (G.wkn FGHO.sucAll (G.Ext x r))))
              
cnvGEnv :: G.Env FG.Typ r -> G.Env (FGHO.Exp r) r  
cnvGEnv  =  mapGEnv . G.cnvGEnvtoGVar
 
mapGEnv :: G.Env (G.Var r) r' -> G.Env (FGHO.Exp r) r'
mapGEnv G.Emp        = G.Emp
mapGEnv (G.Ext x xs) = G.Ext (FGHO.Var x) (mapGEnv xs) 