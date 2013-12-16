{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module EqualityProof where

import Type.GADT
import Environment.GADT
import ErrorMonad

-- Proof of Equality
data Eql a b where
  Rfl :: Eql a a 
  --Reflexivity as a proof of equality

-- Equality of Types
eqlTyp :: Typ t -> Typ t' -> ErrM (Eql t t')
eqlTyp Int         Int           = return Rfl
eqlTyp (Arr t1 t2) (Arr t1' t2') = do Rfl <- eqlTyp t1 t1'
                                      Rfl <- eqlTyp t2 t2'
                                      return Rfl                    
eqlTyp _           _             = fail "Type Error!"

-- Equality of Environments
eqlEnv :: Env e -> Env e' -> ErrM (Eql e e')
eqlEnv Emp         Emp           = return Rfl
eqlEnv (t `Ext` e) (t' `Ext` e') = do Rfl <- eqlEnv e e'
                                      Rfl <- eqlTyp t t'
                                      return Rfl
eqlEnv _           _             = fail "Scope Error!"     
