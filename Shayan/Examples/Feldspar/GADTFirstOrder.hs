{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleContexts, DataKinds, TypeOperators #-}
module Examples.Feldspar.GADTFirstOrder where

import Prelude hiding (abs)
import Expression.Feldspar.GADTFirstOrder
import Variable
import Evaluation as E
import Evaluation.Feldspar.GADTFirstOrder ()
import qualified Expression.Feldspar.GADTValue as V
import Singleton
import Singleton.TypeFeldspar 
import qualified Type.Feldspar as A

-- An example expression doubling the input number                    
dbl :: Exp (A.Int `A.Arr` (A.Int `A.Arr` A.Int) ': '[]) (A.Int `A.Arr` A.Int)
dbl = Abs (app (app (Var (Suc Zro)) (Var Zro)) (Var Zro))

-- An example expression composing two types
compose :: (HasSin Typ ta , HasSin Typ tb , HasSin Typ tc) =>
           Exp r ((tb `A.Arr` tc) `A.Arr` ((ta `A.Arr` tb) 
                  `A.Arr` (ta `A.Arr` tc)))
compose = Abs (Abs (Abs 
                    (Var (Suc (Suc Zro)) `app` (Var (Suc Zro) `app` Var Zro))))

-- An example expression representing the integer 4
four :: Exp (A.Int `A.Arr` (A.Int `A.Arr` A.Int) ': '[]) A.Int
four = (compose `app` dbl `app` dbl) `app` (ConI 1)
 
-- Two simple test cases
test :: Bool
test = evl four (V.addV,()) == return 4
