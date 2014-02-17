{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module SingletonEquality.TypeSTLCGADT where

import Singleton.TypeSTLC
import SingletonEquality

instance EqlSin Typ where
  eqlSin Int         Int           = return Rfl
  eqlSin (Arr t1 t2) (Arr t1' t2') = do Rfl <- eqlSin t1 t1'
                                        Rfl <- eqlSin t2 t2'
                                        return Rfl                    
  eqlSin _            _            = fail "Type Error!"
