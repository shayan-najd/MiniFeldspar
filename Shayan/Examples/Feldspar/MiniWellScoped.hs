{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
module Examples.Feldspar.MiniWellScoped where

import Prelude hiding (abs)
import Expression.Feldspar.MiniWellScoped
import Variable.GADT
import Evaluation as E
import Evaluation.Feldspar.MiniWellScoped ()
import qualified Value.Feldspar.GADT as V
import Singleton
import Singleton.TypeFeldspar ()
import Type.Feldspar.GADT

-- An example expression doubling the input number                    
dbl :: Exp () (Integer -> Integer)
dbl =  undefined -- \ x ->  Var "+" (+)   

-- An example expression composing two types
compose :: Exp () ((tb -> tc) -> (ta -> tb) -> (ta -> tc))
compose =  undefined
  
-- An example expression representing the Integer 4
four :: Exp () Integer
four = undefined -- (compose `App` dbl `App` dbl) `App` (ConI 1)
 
-- Two simple test cases
test :: Bool
test = evl four () == return 4
