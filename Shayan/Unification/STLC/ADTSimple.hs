{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses , TypeFamilies #-}
module Unification.STLC.ADTSimple where

import qualified Type.STLC.ADTSimple as AS
import ErrorMonad
import Unification
import Data.Nat.GADT 
import Data.Vector
import qualified Variable.GADT as V

instance Uni AS.Typ where
  type Mnd     AS.Typ = ErrM 
  type TypCons AS.Typ = (Zro,(Suc (Suc Zro),()))
  
  typCon V.Zro          Nil                 = return AS.Int 
  typCon (V.Suc V.Zro)  (ta ::: tb ::: Nil) = return (AS.Arr ta tb)
  typCon _              _                   = fail "Impossible!"
  
  eql = (AS.===) 
  
  eqlCon V.Zro         AS.Int         = return Nil
  eqlCon (V.Suc V.Zro) (AS.Arr ta tb) = return (ta ::: tb ::: Nil)
  eqlCon _             _              = fail "Impossible!"