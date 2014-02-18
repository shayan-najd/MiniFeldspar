{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
{-# LANGUAGE TypeOperators, DataKinds, PolyKinds #-}
module Expression.STLC.GADTFirstOrder where

import Prelude hiding (sin)
import Variable.GADT
import Singleton.TypeSTLC
import Singleton
import qualified Type.STLC.ADTSimple as A
 
data Exp :: [A.Typ] -> A.Typ -> * where
  Con :: Integer -> Exp r A.Int
  Add :: Exp r A.Int -> Exp r A.Int -> Exp r A.Int
  Var :: Var r t -> Exp r t
  Abs :: Typ ta -> Exp (ta ': r) tb -> Exp r (A.Arr ta tb)
  App :: Exp r (A.Arr ta tb) -> Exp r ta -> Exp r tb
 
abs :: HasSin Typ ta => Exp (ta ': r) tb -> Exp r (A.Arr ta tb) 
abs = Abs sin