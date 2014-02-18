{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies  #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
module Unification.Feldspar.ADTSimple where

import qualified Type.Feldspar.ADTSimple as FS
import Unification
import ErrorMonad
import Variable.GADT 
import Data.Vector
import qualified Data.Nat as N

instance Uni FS.Typ where
  type Mnd FS.Typ = ErrM
  type TypCons FS.Typ = (N.Zro ': N.Suc (N.Suc N.Zro) ': N.Zro ': 
                         N.Suc (N.Suc N.Zro) ': N.Suc N.Zro ': '[])
  typCon Zro                         Nil                 = return FS.Int
  typCon (Suc Zro)                   (ta ::: tb ::: Nil) = return (FS.Arr ta tb)
  typCon (Suc (Suc Zro))             Nil                 = return FS.Bol
  typCon (Suc (Suc (Suc Zro)))       (tf ::: ts ::: Nil) = return (FS.Tpl tf ts)
  typCon (Suc (Suc (Suc (Suc Zro)))) (ta ::: Nil)        = return (FS.Ary ta)
  typCon _                           _                   = fail "Type Error!"
  
  eql = (FS.===)
  
  eqlCon Zro                         FS.Int         = return Nil
  eqlCon (Suc Zro)                   (FS.Arr ta tb) = return (ta ::: tb ::: Nil) 
  eqlCon (Suc (Suc Zro))             FS.Bol         = return Nil
  eqlCon (Suc (Suc (Suc Zro)))       (FS.Tpl tf ts) = return (tf ::: ts ::: Nil) 
  eqlCon (Suc (Suc (Suc (Suc Zro)))) (FS.Ary ta)    = return (ta ::: Nil)
  eqlCon _                           _              = fail "Type Error!"
