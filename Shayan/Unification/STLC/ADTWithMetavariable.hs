{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
module Unification.STLC.ADTWithMetavariable where

import qualified Type.STLC.ADTWithMetavariable as AM
import Unification
import ErrorMonad
import Conversion
import Control.Monad.State(State)
import InferenceMonad
import Variable.ADT
import qualified Conversion.Type.STLC as C

-- Setting the checker to collect constraints wherever types are unified  
instance Uni AM.Typ where
  type Mnd AM.Typ = (State (Nat , [EqlC C.EnvAMH]))
  tCon "Int" []         = return AM.Int
  tCon "Arr" [ta , tb]  = return (AM.Arr ta tb) --
  tCon _     _          = fail "Type Error!"
  eql t1 t2  = do let Rgt t1' = cnv t1 
                      Rgt t2' = cnv t2
                  addC (t1' :~: t2') 
  eqlCon "Int" t = do eql t AM.Int 
                      return []
  eqlCon "Arr" t = do t1 <- newMta
                      t2 <- newMta                 
                      let Rgt t' = cnv t
                          Rgt tr = cnv (t1 `AM.Arr` t2)
                      addC (t' :~: tr)
                      return [t1 , t2]
  eqlCon _    _  = fail "Type Error!"                      
