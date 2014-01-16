{-# OPTIONS_GHC -Wall #-}
module Examples.STLC.ADTChurchMonomorphic where

import Expression.STLC.ADTChurchMonomorphic
import Type.STLC.ADTSimple as T
import Variable.ADT
import qualified Value.STLC.ADT as V
import ErrorMonad
import Evaluation 
import Evaluation.STLC.ADTChurchMonomorphic ()
import TypeChecking.STLC.ADTChurchMonomorphic ()
import TypeChecking  

-- An example expression doubling the input number
dbl :: Exp
dbl = Abs Int (Var Zro `Add` Var Zro)

-- An example expression composing two types
compose :: T.Typ -> T.Typ -> T.Typ -> Exp
compose ta tb tc = Abs (Arr tb tc) 
                (Abs (Arr ta tb) 
                 (Abs ta
                  (Var (Suc (Suc Zro)) `App` (Var (Suc Zro) `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp
four = (compose Int Int Int `App` dbl `App` dbl) `App` (Con 1)

-- Two simple test cases
test :: Bool
test = (case evl four [] of 
          Rgt (V.Con 4) -> True
          _             -> False) 
       && (chk four [] == Rgt Int)