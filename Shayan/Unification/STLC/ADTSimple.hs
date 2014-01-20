{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses , TypeFamilies #-}
module Unification.STLC.ADTSimple where

import qualified Type.STLC.ADTSimple as AS
import ErrorMonad
import Unification

instance Uni AS.Typ where
  type Mnd AS.Typ = ErrM 
  tCon "Int"  []        = return AS.Int
  tCon "Arr"  [ta,tb]   = return (AS.Arr ta tb)
  tCon _      _         = fail "Type Error!"
  
  eql = (AS.===) 
  
  eqlCon "Int" AS.Int         = return []
  eqlCon "Arr" (AS.Arr ta tb) = return [ta , tb]
  eqlCon _     _              = fail "Type Error!"  
