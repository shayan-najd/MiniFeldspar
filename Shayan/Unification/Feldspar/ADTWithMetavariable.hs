{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies  #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
module Unification.Feldspar.ADTWithMetavariable where

import qualified Type.Feldspar.ADTWithMetavariable as FAM
import Unification
import ErrorMonad
import Conversion
import Control.Monad.State(State)
import InferenceMonad
import Conversion.Type.Feldspar ()
import Data.Vector
import Variable.GADT
import qualified Data.Nat as N

-- Setting the checker to collect constraints wherever types are unified  
instance Uni FAM.Typ where
  type Mnd     FAM.Typ = (State (N.Nat , [HerCon (EnvFld '[])]))
  type TypCons FAM.Typ = N.Zro ': N.Suc (N.Suc N.Zro) ': N.Zro
                         ': N.Suc (N.Suc N.Zro) ': N.Suc N.Zro ': '[]
  typCon Zro                         Nil                 = return FAM.Int
  typCon (Suc Zro)                   (ta ::: tb ::: Nil) = return (FAM.Arr ta tb)
  typCon (Suc (Suc Zro))             Nil                 = return FAM.Bol
  typCon (Suc (Suc (Suc Zro)))       (tf ::: ts ::: Nil) = return (FAM.Tpl tf ts)
  typCon (Suc (Suc (Suc (Suc Zro)))) (ta ::: Nil)        = return (FAM.Ary ta)
  typCon _     _          = fail "Type Error!"
  
  eql t1 t2  = do let Rgt t1' = cnv t1 
                      Rgt t2' = cnv t2
                  addC (t1' :~: t2') 
  
  eqlCon Zro                         t = do eql t FAM.Int 
                                            return Nil  
  eqlCon (Suc Zro)                   t = do t1 <- newMta
                                            t2 <- newMta                 
                                            let Rgt t' = cnv t
                                                Rgt tr = cnv (t1 `FAM.Arr` t2)
                                            addC (t' :~: tr)
                                            return (t1 ::: t2 ::: Nil) 
  eqlCon (Suc (Suc Zro))             t =  do eql t FAM.Bol 
                                             return Nil             
  eqlCon (Suc (Suc (Suc Zro)))       t = do tf <- newMta
                                            ts <- newMta                 
                                            let Rgt t' = cnv t
                                                Rgt tr = cnv (FAM.Tpl tf ts)
                                            addC (t' :~: tr)
                                            return (tf ::: ts ::: Nil) 
  eqlCon (Suc (Suc (Suc (Suc Zro)))) t = do ta <- newMta
                                            let Rgt t' = cnv t
                                                Rgt tr = cnv (FAM.Ary ta)
                                            addC (t' :~: tr)
                                            return (ta ::: Nil)                 
  eqlCon _                           _ = fail "Type Error!"