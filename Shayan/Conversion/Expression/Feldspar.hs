{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
           , ScopedTypeVariables, GADTs #-}
module Conversion.Expression.Feldspar where

import qualified Expression.Feldspar.ADTUntypedPolymorphic as FAUP
import qualified Expression.Feldspar.ADTUntypedMonomorphic as FAUM
import qualified Expression.Feldspar.ADTChurchPolymorphic  as FACP
import qualified Expression.Feldspar.GADT                  as FGDT
 
import qualified Type.Feldspar.ADTSimple           as FAS
import qualified Type.Feldspar.ADTWithMetavariable as FAM
import qualified Type.Feldspar.GADT                as G

import qualified Variable.ADT            as A

import qualified Environment.ADT         as A
import qualified Environment.ADTTable    as AT
import qualified Environment.GADT        as G

import qualified Conversion.Type.Feldspar as C
import Conversion.Type.Feldspar ()
import Conversion.Variable ()
import Conversion.Environment ()
import Conversion.Existential ()

import SingletonEquality.EnvironmentGADT ()
import SingletonEquality.TypeFeldsparGADT ()

import TypeChecking.Feldspar.ADTChurchPolymorphic ()
import Unification.Feldspar.ADTWithMetavariable ()

import Conversion
import ErrorMonad
import Inference
import InferenceMonad
import Existential
import SingletonEquality
import Singleton

import Data.Traversable(traverse)
import Control.Applicative ((<$>),(<*>))

type ExsTyp = ExsSin G.Typ
type ExsExp = Exs2 FGDT.Exp (G.Env G.Typ) G.Typ

---------------------------------------------------------------------------------
-- Conversion from FAUP
---------------------------------------------------------------------------------
instance (Eq x , Sin (G.Env G.Typ) r , Sin G.Typ t , Cnv t FAM.Typ) => 
         Cnv (FAUP.Exp x , AT.Env x FAS.Typ) (FGDT.Exp r t)  where
  cnv (e , r) = do e' :: ExsExp <- cnv (e , r)
                   cnv e' 

instance (Eq x , Sin (G.Env G.Typ) r , Sin G.Typ t) => 
         Cnv (FAUP.Exp x , AT.Env x FAS.Typ , AT.Env x FAUM.Exp) (FGDT.Exp r t)  where
  cnv (e , rt , rs) = do e' :: FAUM.Exp <- cnv (e , rt , rs)
                         cnv (e', map snd rt)

instance (Eq x , Cnv t FAM.Typ) => 
         Cnv (FAUP.Exp x , AT.Env x t) ExsExp  where
  cnv (e , r) = do r' :: AT.Env x FAM.Typ <- cnvEnvAMAS r
                   e' :: FACP.Exp FAM.Typ <- cnv (e  , r)
                   cnv (e' , map snd r')                   

instance (Eq x , Cnv t FAM.Typ , Cnv FAM.Typ t') => 
         Cnv (FAUP.Exp x , AT.Env x t) (FACP.Exp t') where
  cnv (e , rt) = do r' :: AT.Env x FAM.Typ <- cnvEnvAMAS rt  
                    e' :: FAUM.Exp         <- cnv (e   , r')            
                    cnv (e'  , map snd r')

instance (Eq x , Cnv t FAM.Typ) => 
         Cnv (FAUP.Exp x , AT.Env x t) FAUM.Exp where
  cnv (e , rt) = cnv (e , rt , [] :: AT.Env x FAUM.Exp)
 
instance (Eq x , Cnv t FAM.Typ) => 
         Cnv (FAUP.Exp x , AT.Env x t , AT.Env x FAUM.Exp) FAUM.Exp where
  cnv (e , rt , rf) = do rt' :: AT.Env x FAM.Typ <- cnvEnvAMAS rt 
                         cnvExpUUToU e (zip (map fst rt') [A.Zro ..]) rf
                         
---------------------------------------------------------------------------------
-- Conversion from FAUM
---------------------------------------------------------------------------------
instance (Sin (G.Env G.Typ) r , Sin G.Typ t , Cnv t' FAM.Typ) => 
         Cnv (FAUM.Exp , A.Env t') (FGDT.Exp r t)  where
  cnv (e , r) = do e' :: ExsExp <- cnv (e , r)       
                   cnv e'

instance Cnv t FAM.Typ => 
         Cnv (FAUM.Exp, A.Env t) ExsExp where
  cnv (e , r) = do r' :: A.Env FAM.Typ    <- cnv r 
                   e' :: FACP.Exp FAM.Typ <- cnv (e , r')
                   cnv (e' , r')

instance (Cnv t FAM.Typ , Cnv FAM.Typ t') => 
          Cnv (FAUM.Exp , A.Env t) (FACP.Exp t') where
  cnv (e , r) = do r' <- cnv r
                   e' <- inf cnvExpUToACPAM (e , r')
                   traverse cnv e'

---------------------------------------------------------------------------------
-- Conversion from FACP
---------------------------------------------------------------------------------
instance (Sin (G.Env G.Typ) r , Sin G.Typ t' , Cnv t' FAM.Typ, t ~ t') => 
         Cnv (FACP.Exp t, A.Env t) (FGDT.Exp r t')  where
  cnv (e , r) = do r'  :: A.Env    FAM.Typ <- cnv r
                   e'  :: FACP.Exp FAM.Typ <- traverse cnv e
                   e'' :: ExsExp           <- cnv (e' , r')       
                   cnv e''

instance Cnv (FACP.Exp FAS.Typ , A.Env FAS.Typ) ExsExp where
  cnv (e , r) = do r' :: A.Env FAM.Typ    <- cnv r
                   e' :: FACP.Exp FAM.Typ <- traverse cnv e
                   cnv (e' , r') 

instance Cnv (FACP.Exp FAM.Typ , A.Env FAM.Typ) ExsExp where
  cnv (FACP.ConI i , r) = do ExsSin r' <- cnv r
                             return (Exs2 (FGDT.ConI i) r' G.Int)
  
  cnv (FACP.ConB i , r) = do ExsSin r' <- cnv r
                             return (Exs2 (FGDT.ConB i) r' G.Bol)
  
  cnv (FACP.Var x  , r) = do Exs2 x' r' t' <- cnv (x , r)
                             return (Exs2 (FGDT.Var x') r' t')

  cnv (FACP.Abs ta eb , r) = do ExsSin ta'   :: ExsTyp <- cnv ta
                                Exs2 eb' (ta'' `G.Ext` r') tb 
                                             :: ExsExp <- cnv(eb , ta : r)
                                Rfl <- eqlSin ta' ta''
                                return (Exs2 (FGDT.Abs eb') r' 
                                             (G.Arr ta' tb))

  cnv (FACP.App ef ea , r) = do Exs2 ef' rf (G.Arr ta tb) 
                                             :: ExsExp <- cnv (ef , r)
                                Exs2 ea' ra ta'           
                                             :: ExsExp <- cnv (ea , r)
                                Rfl <- eqlSin rf ra
                                Rfl <- eqlSin ta ta'
                                return (Exs2 (FGDT.App ef' ea') rf tb)
 
  cnv (FACP.Cnd ec et ef , r) = do Exs2 ec' rc G.Bol 
                                             :: ExsExp <- cnv (ec , r)
                                   Exs2 et' rt tt           
                                             :: ExsExp <- cnv (et , r)
                                   Exs2 ef' rf tf           
                                             :: ExsExp <- cnv (ef , r)
                                   Rfl <- eqlSin rc rt
                                   Rfl <- eqlSin rc rf
                                   Rfl <- eqlSin tt tf
                                   return (Exs2 (FGDT.Cnd ec' et' ef') rc tt)
  
  cnv (FACP.Whl ec eb ei , r) = do Exs2 ec' rc (G.Arr tac G.Bol) 
                                             :: ExsExp <- cnv (ec , r)
                                   Exs2 eb' rb (G.Arr tab tbb)           
                                             :: ExsExp <- cnv (eb , r)
                                   Exs2 ei' ri ti           
                                             :: ExsExp <- cnv (ei , r)
                                   Rfl <- eqlSin rc rb
                                   Rfl <- eqlSin rc ri
                                   Rfl <- eqlSin tac tab
                                   Rfl <- eqlSin tac tbb
                                   Rfl <- eqlSin tac ti
                                   return (Exs2 (FGDT.Whl ec' eb' ei') rc tac)
  
  cnv (FACP.Tpl ef es , r) = do Exs2 ef' rf tf
                                             :: ExsExp <- cnv (ef , r)
                                Exs2 es' rs ts           
                                             :: ExsExp <- cnv (es , r)
                                Rfl <- eqlSin rf rs
                                Rfl <- eqlSin tf ts
                                return (Exs2 (FGDT.Tpl ef' es') rf (G.Tpl tf ts))

  cnv (FACP.Fst e , r)     = do Exs2 e' r' (G.Tpl tf _)
                                             :: ExsExp <- cnv (e , r)
                                return (Exs2 (FGDT.Fst e') r' tf)
  
  cnv (FACP.Snd e , r)     = do Exs2 e' r' (G.Tpl _ ts)
                                             :: ExsExp <- cnv (e , r)
                                return (Exs2 (FGDT.Snd e') r' ts)
  
  cnv (FACP.Ary el ef , r) = do Exs2 el' rl G.Int
                                             :: ExsExp <- cnv (el , r)
                                Exs2 ef' rf (G.Arr G.Int ta)           
                                             :: ExsExp <- cnv (ef , r)
                                Rfl <- eqlSin rl rf
                                return (Exs2 (FGDT.Ary el' ef') rl (G.Ary ta))
  
  cnv (FACP.Len e , r)     = do Exs2 e' r' (G.Ary _)
                                             :: ExsExp <- cnv (e , r)
                                return (Exs2 (FGDT.Len e') r' G.Int)
  
  cnv (FACP.Ind ea ei , r) = do Exs2 ea' ra (G.Ary ta)
                                             :: ExsExp <- cnv (ea , r)
                                Exs2 ei' ri G.Int            
                                             :: ExsExp <- cnv (ei , r)
                                Rfl <- eqlSin ra ri
                                return (Exs2 (FGDT.Ind ea' ei') ra ta)
    
  cnv (FACP.Let el eb , r) = do Exs2 el' rl tl :: ExsExp  <- cnv (el , r)
                                tl'            :: FAM.Typ <- cnv tl
                                Exs2 eb' (G.Ext  tl'' rb) tb           
                                               :: ExsExp  <- cnv (eb , tl' : r)
                                Rfl                       <- eqlSin tl tl''
                                Rfl                       <- eqlSin rb rl
                                return (Exs2 (FGDT.Let el' eb') rb tb) 

 
cnvEnvAMAS :: Cnv a b => AT.Env x a -> ErrM (AT.Env x b)
cnvEnvAMAS = mapM (\(x , y) -> do y' <- cnv y
                                  return (x , y'))
  
-- Conversion from the untyped lambda calculus to the explicitly typed 
-- simply-typed lambda calculus (church style) where every explicit type 
-- annotaion is set as a fresh metavariable
cnvExpUToACPAM :: FAUM.Exp -> InfM C.EnvFAMH  (FACP.Exp FAM.Typ)
cnvExpUToACPAM (FAUM.ConI i)    = return (FACP.ConI i)
cnvExpUToACPAM (FAUM.ConB i)    = return (FACP.ConB i)
cnvExpUToACPAM (FAUM.Var v)     = return (FACP.Var v)
cnvExpUToACPAM (FAUM.Abs eb)    = FACP.Abs <$> newMta <*> cnvExpUToACPAM eb
cnvExpUToACPAM (FAUM.App ef ea) = FACP.App <$> cnvExpUToACPAM ef <*> 
                                  cnvExpUToACPAM ea
cnvExpUToACPAM (FAUM.Cnd ec et ef) = FACP.Cnd <$> cnvExpUToACPAM ec <*> 
                                     cnvExpUToACPAM et <*>          
                                     cnvExpUToACPAM ef 
cnvExpUToACPAM (FAUM.Whl ec eb ei) = FACP.Whl <$> cnvExpUToACPAM ec <*> 
                                     cnvExpUToACPAM eb <*>          
                                     cnvExpUToACPAM ei
cnvExpUToACPAM (FAUM.Tpl ef es)    = FACP.Tpl <$> cnvExpUToACPAM ef <*> 
                                     cnvExpUToACPAM es
cnvExpUToACPAM (FAUM.Ary el ef)    = FACP.Ary <$> cnvExpUToACPAM el <*> 
                                     cnvExpUToACPAM ef
cnvExpUToACPAM (FAUM.Ind ea ei)    = FACP.Ind <$> cnvExpUToACPAM ea <*> 
                                     cnvExpUToACPAM ei
cnvExpUToACPAM (FAUM.Fst e)        = FACP.Fst <$> cnvExpUToACPAM e
cnvExpUToACPAM (FAUM.Snd e)        = FACP.Snd <$> cnvExpUToACPAM e
cnvExpUToACPAM (FAUM.Len e)        = FACP.Len <$> cnvExpUToACPAM e 
--cnvExpUToACPAM FAUM.Any            = return FACP.Any
cnvExpUToACPAM (FAUM.Let el eb)    = FACP.Let <$> cnvExpUToACPAM el  <*> 
                                     cnvExpUToACPAM eb

cnvExpUUToU :: Eq x => 
               FAUP.Exp x -> AT.Env x A.Nat -> AT.Env x FAUM.Exp -> ErrM FAUM.Exp
cnvExpUUToU (FAUP.ConI i)    _  _  = return (FAUM.ConI i)
cnvExpUUToU (FAUP.ConB i)    _  _  = return (FAUM.ConB i)
cnvExpUUToU (FAUP.Var s)     rb rf = case (AT.get s rb , AT.get s rf) of
  (Just x  , _)                   -> return (FAUM.Var x)
  (Nothing , Just e)              -> return e
  _                               -> fail "Scope Error!"
cnvExpUUToU (FAUP.Abs s  eb) rb rf = FAUM.Abs <$> 
                                     cnvExpUUToU eb ((s , A.Zro) : 
                                                     fmap A.Suc `map` rb) rf 

cnvExpUUToU (FAUP.Let s el eb) rb rf = FAUM.Let <$> 
                                       cnvExpUUToU el rb rf <*>
                                       cnvExpUUToU eb ((s , A.Zro) : 
                                                       fmap A.Suc `map` rb) rf 

cnvExpUUToU (FAUP.App ef ea) rb rf = FAUM.App <$>
                                     cnvExpUUToU ef rb rf <*>
                                     cnvExpUUToU ea rb rf 

cnvExpUUToU (FAUP.Cnd ec et ef) rb rf = FAUM.Cnd <$> 
                                        cnvExpUUToU ec rb rf <*> 
                                        cnvExpUUToU et rb rf <*> 
                                        cnvExpUUToU ef rb rf   

cnvExpUUToU (FAUP.Whl ec eb ei) rb rf = FAUM.Whl <$> 
                                        cnvExpUUToU ec rb rf <*> 
                                        cnvExpUUToU eb rb rf <*> 
                                        cnvExpUUToU ei rb rf 

cnvExpUUToU (FAUP.Tpl ef es) rb rf = FAUM.Tpl <$> 
                                     cnvExpUUToU ef rb rf <*> 
                                     cnvExpUUToU es rb rf   

cnvExpUUToU (FAUP.Fst e) rb rf = FAUM.Fst <$> 
                                 cnvExpUUToU e rb rf 

cnvExpUUToU (FAUP.Snd e) rb rf = FAUM.Snd <$> 
                                 cnvExpUUToU e rb rf 
                                 

cnvExpUUToU (FAUP.Ary el ef) rb rf = FAUM.Ary <$> 
                                     cnvExpUUToU el rb rf <*> 
                                     cnvExpUUToU ef rb rf   
                                     
cnvExpUUToU (FAUP.Ind ea ei) rb rf = FAUM.Ind <$> 
                                     cnvExpUUToU ea rb rf <*> 
                                     cnvExpUUToU ei rb rf   

cnvExpUUToU (FAUP.Len e) rb rf = FAUM.Len <$> 
                                 cnvExpUUToU e rb rf 

-- cnvExpUUToU FAUP.Any     _  _  = return FAUM.Any
