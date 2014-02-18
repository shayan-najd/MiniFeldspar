{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs, PolyKinds #-}
module SingletonEquality.EnvironmentGADT where

import Singleton.Environment
import SingletonEquality

instance EqlSin tf => EqlSin (Env tf) where 
  eqlSin Emp       Emp         = return Rfl
  eqlSin (Ext t e) (Ext t' e') = do Rfl <- eqlSin e e'
                                    Rfl <- eqlSin t t'
                                    return Rfl
  eqlSin  _           _        = fail "Scope Error!"     
