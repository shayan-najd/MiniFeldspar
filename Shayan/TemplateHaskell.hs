{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, GADTs, FlexibleInstances
           , NoMonomorphismRestriction, MultiParamTypeClasses #-}

import qualified Expression.ADTUntypedUnscoped   as UU
import qualified Expression.ADTUntyped           as U
import qualified Expression.Existential          as W
import qualified Expression.GADT                 as G
import qualified Language.Haskell.TH.Syntax      as TH
import qualified Type.ADTSimple                  as AS
import qualified Type.GADT                       as G
import qualified Environment.ADT                 as A
import qualified Environment.GADT                as G
import Expression.Conversion ()
import Type.Singleton 
import Environment.Singleton 
import Conversion   
import ErrorMonad
import EqualityProof
import Singleton

import Prelude hiding (filter)
import Control.Monad ((<=<))

instance TH.Quasi ErrM where
  qNewName = return . TH.mkName 
 
instance Cnv TH.Exp (UU.Exp TH.Name) where
  cnv (TH.LitE (TH.IntegerL i))     = return (UU.Con (fromInteger i))
  cnv (TH.VarE n)                   = return (UU.Var n)
  cnv (TH.LamE (TH.VarP x : []) eb) = do eb' <- cnv eb 
                                         return (UU.Abs x eb')
                                         
  cnv (TH.AppE ef ea)               = do ef' <- cnv ef 
                                         ea' <- cnv ea 
                                         return (UU.App ef' ea')
  cnv (TH.InfixE 
       (Just el) 
       (TH.VarE 
        (TH.Name (TH.OccName "+") 
         (TH.NameG TH.VarName (TH.PkgName "base") 
          (TH.ModName "GHC.Num")))) 
       (Just er))                   = do el' <- cnv el   
                                         er' <- cnv er 
                                         return (UU.Add el' er')
  cnv _                             = fail "Syntax Error!"

instance Cnv (TH.Exp , [(TH.Name , U.Exp)]) W.Exp where
  cnv (e , r) = do e'  :: UU.Exp TH.Name <- cnv e 
                   e'' :: U.Exp          <- cnv (e' , r)
                   cnv (e'' , [] :: A.Env AS.Typ)

instance (SigTyp t , SigEnv e) => Cnv W.Exp (G.Exp e t) where
  cnv (W.Exp e r t) = do t' :: G.Typ t <- sigTyp Prx 
                         r' :: G.Env e <- sigEnv Prx 
                         Rfl <- eqlTyp t t'     
                         Rfl <- eqlEnv r r'
                         return e

instance (SigTyp t , SigEnv e , t ~ t') => 
         Cnv (TH.Q (TH.TExp t) , [(TH.Name , U.Exp)]) (G.Exp e t') where
          cnv (e , r) = (cnv 
                         <=< norm 
                         <=< flip (curry cnv) r
                         <=< (TH.runQ . TH.unTypeQ)) e
 
-- specialization for guiding the type inference
cnvQ :: SigTyp t => 
        (TH.Q (TH.TExp t) , [(TH.Name , U.Exp)]) -> ErrM (G.Exp () t)
cnvQ = cnv               

norm :: W.Exp -> ErrM W.Exp
norm = return 
   
tst0THQ = [|| 1 ||]

tst0Exp = cnvQ (tst0THQ , [])

tst1THQ = [|| \ x -> $$tst0THQ + x ||]

tst1Exp = cnvQ (tst1THQ , [])

-- Vanilla
tst0Van :: Integer
tst0Van = undefined

tst2THQ = [|| \ x -> tst0Van + x ||]

tst2Exp' = cnvQ (tst2THQ , [])

tst2Exp  = cnvQ (tst2THQ , [('tst0Van , U.Con 1)])
 