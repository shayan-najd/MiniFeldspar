{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module TypeChecking where

import Unification

class Uni (Typ e) => Chk (e :: *) where
  type Env e :: *
  type Typ e :: *
  chk :: e -> Env e -> Mnd (Typ e) (Typ e)