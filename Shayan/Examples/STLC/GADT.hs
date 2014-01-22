{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Examples.STLC.GADT where

import Expression.STLC.GADT
import Variable.GADT
import Evaluation as E
import Evaluation.STLC.GADT ()

-- An example expression doubling the input number                    
dbl :: Exp () (Integer -> Integer)
dbl = Abs (Var Zro `Add` Var Zro)

-- An example expression composing two types
compose :: Exp () ((tb -> tc) -> (ta -> tb) -> (ta -> tc))
compose = Abs (Abs (Abs
                    (Var (Suc (Suc Zro)) `App` (Var (Suc Zro) `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp () Integer
four = (compose `App` dbl `App` dbl) `App` (Con 1)
 
-- Two simple test cases
test :: Bool
test = evl four () == return 4
 