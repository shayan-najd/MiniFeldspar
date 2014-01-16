{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances  #-}
module Unification.Feldspar.ADTWithMetavariable where

import qualified Type.Feldspar.ADTWithMetavariable as FAM
import Unification
import ErrorMonad
import Conversion
import Control.Monad.State(State)
import InferenceMonad
import qualified Conversion.Type.Feldspar as C
import Variable.ADT

-- Setting the checker to collect constraints wherever types are unified  
instance Uni FAM.Typ (State (Nat , [EqlC C.EnvFAMH])) where
  tCon "Int" []         = return FAM.Int
  tCon "Bol" []         = return FAM.Bol
  tCon "Arr" [ta , tb]  = return (FAM.Arr ta tb)
  tCon "Tpl" [tf , ts]  = return (FAM.Tpl tf ts)
  tCon "Ary" [t]        = return (FAM.Ary t)
  tCon _     _          = fail "Type Error!"
  eql t1 t2  = do let Rgt t1' = cnv t1 
                      Rgt t2' = cnv t2
                  addC (t1' :~: t2') 
  eqlCon "Int" t = do eql t FAM.Int 
                      return []
  eqlCon "Bol" t = do eql t FAM.Bol 
                      return []                      
  eqlCon "Arr" t = do t1 <- newMta
                      t2 <- newMta                 
                      let Rgt t' = cnv t
                          Rgt tr = cnv (t1 `FAM.Arr` t2)
                      addC (t' :~: tr)
                      return [t1 , t2]
  eqlCon "Tpl" t = do tf <- newMta
                      ts <- newMta                 
                      let Rgt t' = cnv t
                          Rgt tr = cnv (FAM.Tpl tf ts)
                      addC (t' :~: tr)
                      return [tf , ts]
  eqlCon "Ary" t = do ta <- newMta
                      let Rgt t' = cnv t
                          Rgt tr = cnv (FAM.Ary ta)
                      addC (t' :~: tr)
                      return [ta]                      
  eqlCon _    _  = fail "Type Error!"                      
