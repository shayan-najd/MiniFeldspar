{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Examples.STLC.GADT where

import Expression.STLC.GADT
import Type.STLC.GADT as T
import Variable.GADT
import Environment.GADT as G
import Evaluation as E
import Evaluation.STLC.GADT ()
import TypeChecking.STLC.GADT () 
import TypeChecking
import ErrorMonad

-- An example expression doubling the input number                    
dbl :: Exp () (Integer -> Integer)
dbl = Abs Int (Var Zro `Add` Var Zro)

-- An example expression composing two types
compose :: T.Typ ta -> T.Typ tb -> T.Typ tc -> 
           Exp () ((tb -> tc) -> (ta -> tb) -> (ta -> tc))
compose ta tb tc = Abs (tb `Arr` tc) (Abs (ta `Arr` tb) (Abs ta
                  (Var (Suc (Suc Zro)) `App` (Var (Suc Zro) `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp () Integer
four = (compose Int Int Int `App` dbl `App` dbl) `App` (Con 1)
 
-- Two simple test cases
test :: Bool
test = (evl four () == return 4) 
       && (case chk four Emp of 
             Rgt Int -> True
             Lft _   -> False)
