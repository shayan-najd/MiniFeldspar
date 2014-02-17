{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
module Examples.Feldspar.GADTHigherOrder where

import Prelude hiding (abs)
import Expression.Feldspar.GADTHigherOrder
import Variable.GADT
import Evaluation as E
import Evaluation.Feldspar.GADTHigherOrder ()
import qualified Value.Feldspar.GADT as V
import Singleton
import Singleton.TypeFeldspar ()
import Singleton.TypeFeldspar

-- An example expression doubling the input number                    
dbl :: Exp (Integer -> Integer -> Integer , ()) (Integer -> Integer)
dbl = abs (\ x -> (App (App (Var Zro) x) x))

-- An example expression composing two types
compose :: (HasSin Typ ta , HasSin Typ tb , HasSin Typ tc) => 
           Exp r ((tb -> tc) -> (ta -> tb) -> (ta -> tc))
compose = abs (\ g -> abs (\ f -> abs 
                    (\ x -> g `App` (f `App` x))))

-- An example expression representing the Integer 4
four :: Exp (Integer -> Integer -> Integer , ()) Integer
four = (compose `App` dbl `App` dbl) `App` (ConI 1)
 
-- Two simple test cases
test :: Bool
test = evl four (V.addV,()) == return 4
