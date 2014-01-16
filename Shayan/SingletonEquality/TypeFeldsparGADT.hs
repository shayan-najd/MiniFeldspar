{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module SingletonEquality.TypeFeldsparGADT where

import Type.Feldspar.GADT
import SingletonEquality

instance EqlSin Typ where
  eqlSin Int         Int           = return Rfl
  eqlSin Bol         Bol           = return Rfl
  eqlSin (Arr ta tb) (Arr ta' tb') = do Rfl <- eqlSin ta ta'
                                        Rfl <- eqlSin tb tb'
                                        return Rfl
  eqlSin (Tpl tf ts) (Tpl tf' ts') = do Rfl <- eqlSin tf tf'
                                        Rfl <- eqlSin ts ts'
                                        return Rfl
  eqlSin (Ary t)     (Ary t')      = do Rfl <- eqlSin t t'
                                        return Rfl  
  eqlSin _              _          = fail "Type Error!"
