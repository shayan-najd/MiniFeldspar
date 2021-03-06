module TypeChecking where

import Type.Herbrand
import InferenceMonad
import Nat.ADT

class Chk (ef :: * -> *) where
  type Cns ef :: [Nat]
  type Env ef :: * -> *
  chk :: ef (Typ (Cns ef)) -> (Env ef) (Typ (Cns ef)) ->
         InfM (Cns ef) (Typ (Cns ef))