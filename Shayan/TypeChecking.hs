{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, DataKinds, PolyKinds #-}
module TypeChecking where

import Type.Herbrand
import InferenceMonad
import Data.Nat
import Environment.ADT

class Chk (ef :: * -> *) where
  type Cns ef :: [Nat]    
  chk :: ef (Typ (Cns ef)) -> Env (Typ (Cns ef))-> InfM (Cns ef) (Typ (Cns ef))
  
