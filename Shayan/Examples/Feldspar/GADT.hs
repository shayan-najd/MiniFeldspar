{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Examples.Feldspar.GADT where

import Expression.Feldspar.GADT
import Variable.GADT
import Evaluation as E
import Evaluation.Feldspar.GADT ()
import qualified Value.Feldspar.GADT as V

-- An example expression doubling the input number                    
dbl :: Exp (Integer -> Integer -> Integer , ()) (Integer -> Integer)
dbl = Abs (App (App (Var (Suc Zro)) (Var Zro)) (Var Zro))

-- An example expression composing two types
compose :: Exp r ((tb -> tc) -> (ta -> tb) -> (ta -> tc))
compose = Abs (Abs (Abs 
                    (Var (Suc (Suc Zro)) `App` (Var (Suc Zro) `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp (Integer -> Integer -> Integer , ()) Integer
four = (compose `App` dbl `App` dbl) `App` (ConI 1)
 
-- Two simple test cases
test :: Bool
test = evl four (V.addV,()) == return 4
