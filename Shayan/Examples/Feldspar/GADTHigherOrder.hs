{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleContexts, DataKinds, TypeOperators #-}
module Examples.Feldspar.GADTHigherOrder where

import Prelude hiding (abs)
import Expression.Feldspar.GADTHigherOrder
import Variable
import Evaluation as E
import Evaluation.Feldspar.GADTHigherOrder ()
import qualified Expression.Feldspar.GADTValue as V
import Singleton
import Singleton.TypeFeldspar ()
import Singleton.TypeFeldspar
import qualified Type.Feldspar as A

-- An example expression doubling the input number                    
dbl :: Exp (A.Arr A.Int (A.Arr A.Int A.Int) ': '[]) (A.Arr A.Int A.Int)
dbl = Abs (\ x -> (app (app (Var Zro) x) x))

-- An example expression composing two types
compose :: (HasSin Typ ta , HasSin Typ tb , HasSin Typ tc) => 
           Exp r (A.Arr (A.Arr tb tc) (A.Arr (A.Arr ta tb) (A.Arr ta tc)))
compose = Abs (\ g -> Abs (\ f -> Abs 
                    (\ x -> g `app` (f `app` x))))

-- An example expression representing the Integer 4
four :: Exp (A.Int `A.Arr` (A.Int `A.Arr` A.Int) ': '[]) A.Int
four = (compose `app` dbl `app` dbl) `app` (ConI 1)
 
-- Two simple test cases
test :: Bool
test = evl four (V.addV,()) == return 4
