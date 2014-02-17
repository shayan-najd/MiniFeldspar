{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs , FlexibleContexts #-}
module Examples.STLC.GADTFirstOrder where

import Prelude hiding (abs)
import Expression.STLC.GADTFirstOrder
import Variable.GADT
import Evaluation as E
import Evaluation.STLC.GADTFirstOrder ()
import Singleton
import Singleton.TypeSTLC

-- An example expression doubling the input number                    
dbl :: Exp () (Integer -> Integer)
dbl = abs (Var Zro `Add` Var Zro)

-- An example expression composing two types
compose :: (HasSin Typ ta , HasSin Typ tb , HasSin Typ tc) => 
           Exp () ((tb -> tc) -> (ta -> tb) -> (ta -> tc))
compose = abs (abs (abs
                    (Var (Suc (Suc Zro)) `App` (Var (Suc Zro) `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp () Integer
four = (compose `App` dbl `App` dbl) `App` (Con 1)
 
-- Two simple test cases
test :: Bool
test = evl four () == return 4
 