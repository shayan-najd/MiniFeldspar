{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs , FlexibleContexts, DataKinds, TypeOperators #-}
module Examples.STLC.GADTFirstOrder where

import Prelude hiding (abs)
import Expression.STLC.GADTFirstOrder
import Variable.GADT
import Evaluation as E
import Evaluation.STLC.GADTFirstOrder ()
import Singleton
import Singleton.TypeSTLC 
import qualified Type.STLC as A

-- An example expression doubling the input number                    
dbl :: Exp '[] (A.Arr A.Int A.Int)
dbl = abs (Var Zro `Add` Var Zro)

-- An example expression composing two types
compose :: (HasSin Typ ta , HasSin Typ tb , HasSin Typ tc) => 
           Exp '[] ((A.Arr tb tc) `A.Arr` 
           ((ta `A.Arr` tb) `A.Arr` (ta `A.Arr` tc)))
compose = abs (abs (abs
                    (Var (Suc (Suc Zro)) `App` (Var (Suc Zro) `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp '[] A.Int 
four = (compose `App` dbl `App` dbl) `App` (Con 1)
 
-- Two simple test cases
test :: Bool
test = evl four () == return 4
 