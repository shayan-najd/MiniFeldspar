{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
module Examples.Feldspar.GADTFirstOrder where

import Prelude hiding (abs)
import Expression.Feldspar.GADTFirstOrder
import Variable.GADT
import Evaluation as E
import Evaluation.Feldspar.GADTFirstOrder ()
import qualified Value.Feldspar.GADT as V
import Singleton
import Singleton.TypeFeldspar 

-- An example expression doubling the input number                    
dbl :: Exp (Integer -> Integer -> Integer , ()) (Integer -> Integer)
dbl = abs (App (App (Var (Suc Zro)) (Var Zro)) (Var Zro))

-- An example expression composing two types
compose :: (HasSin Typ ta , HasSin Typ tb , HasSin Typ tc) =>
           Exp r ((tb -> tc) -> (ta -> tb) -> (ta -> tc))
compose = abs (abs (abs 
                    (Var (Suc (Suc Zro)) `App` (Var (Suc Zro) `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp (Integer -> Integer -> Integer , ()) Integer
four = (compose `App` dbl `App` dbl) `App` (ConI 1)
 
-- Two simple test cases
test :: Bool
test = evl four (V.addV,()) == return 4
