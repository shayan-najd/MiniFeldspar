{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs , FlexibleContexts #-}
module Examples.STLC.GADTHigherOrder where

import Prelude hiding (sin,abs)
import Expression.STLC.GADTHigherOrder
import Evaluation as E
import Evaluation.STLC.GADTHigherOrder ()
import Type.STLC.GADT
import Singleton
 
-- An example expression doubling the input number                    
dbl :: Exp () (Integer -> Integer)
dbl = abs (\ e -> e `Add` e)

-- An example expression composing two types
compose :: (HasSin Typ ta , HasSin Typ tb , HasSin Typ tc) => 
           Exp () ((tb -> tc) -> (ta -> tb) -> (ta -> tc))
compose = abs (\ g -> 
                abs (\ f -> 
                      abs (\ x -> g `App` (f `App` x)))) 

-- An example expression representing the Integer 4
four :: Exp () Integer
four = (compose `App` dbl `App` dbl) 
       `App` (Con 1)
 
-- Two simple test cases
test :: Bool
test = evl four () == return 4
 