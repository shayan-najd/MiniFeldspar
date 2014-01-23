{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
module Unification.STLC.ADTWithMetavariable where

import qualified Type.STLC.ADTWithMetavariable as AM
import Unification
import ErrorMonad
import Conversion
import Control.Monad.State(State)
import InferenceMonad
import Variable.GADT
import Data.Vector
import qualified Data.Nat as NN
import Conversion.Type.STLC ()

-- Setting the checker to collect constraints wherever types are unified  
instance Uni AM.Typ where
  type Mnd AM.Typ     = (State (NN.Nat , [EqlC (EnvIntArr ())]))
  type TypCons AM.Typ = (EnvIntArr ())
  typCon Zro       Nil                 = return AM.Int
  typCon (Suc Zro) (ta ::: tb ::: Nil) = return (AM.Arr ta tb)
  typCon _         _                   = fail "Type Error!"
  
  eql t1 t2  = do let Rgt t1' = cnv t1 
                      Rgt t2' = cnv t2
                  addC (t1' :~: t2') 
  
  eqlCon Zro       t = do eql t AM.Int 
                          return Nil
  eqlCon (Suc Zro) t = do ta <- newMta
                          tb <- newMta                 
                          let Rgt t' = cnv t
                              Rgt tr = cnv (ta `AM.Arr` tb)
                          addC (t' :~: tr)
                          return (ta ::: tb ::: Nil)
  eqlCon _         _  = fail "Type Error!"                      
