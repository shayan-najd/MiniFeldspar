{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs , FlexibleContexts, DataKinds, TypeOperators #-}
module Examples.STLC.GADTHigherOrder where

import Prelude hiding (sin,abs)
import Expression.STLC.GADTHigherOrder
import Evaluation as E
import Evaluation.STLC.GADTHigherOrder ()
import Singleton.TypeSTLC
import Singleton
import qualified Type.STLC.ADTSimple as A
 
-- An example expression doubling the input number                    
dbl :: Exp '[] (A.Arr A.Int A.Int)
dbl = abs (\ e -> e `Add` e)

-- An example expression composing two types
compose :: (HasSin Typ ta , HasSin Typ tb , HasSin Typ tc) => 
           Exp '[] ((A.Arr tb tc) `A.Arr` ((A.Arr ta tb) `A.Arr` (A.Arr ta tc)))
compose = abs (\ g -> 
                abs (\ f -> 
                      abs (\ x -> g `App` (f `App` x)))) 

-- An example expression representing the Integer 4
four :: Exp '[] A.Int
four = (compose `App` dbl `App` dbl) 
       `App` (Con 1)
 
-- Two simple test cases
test :: Bool
test = evl four () == return 4
 