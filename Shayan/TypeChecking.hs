{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
module TypeChecking where

import GHC.Exts (Constraint)

class Chk (e :: *) where
  type Env e :: *
  type Mnd e :: (* -> *) -> Constraint
  type Typ e :: *
  chk :: Mnd e m => e -> Env e -> m (Typ e)