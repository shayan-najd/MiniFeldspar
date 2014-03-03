{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
module Examples.Feldspar.GADTUntypedDebruijn where
 
import Expression.Feldspar.GADTUntypedDebruijn
import Data.Fin
import qualified Data.Nat                     as A
import qualified Expression.Feldspar.ADTValue as V
import ErrorMonad
import Evaluation  
import Evaluation.Feldspar.GADTUntypedDebruijn ()
import Data.Vector
 
-- An example expression doubling the input number
dbl :: Exp (A.Suc A.Zro)
dbl = Abs (App (App (Var (Suc Zro)) (Var Zro)) (Var Zro))

-- An example expression composing two types
compose :: Exp (A.Suc A.Zro)
compose = Abs (Abs (Abs  (Var (Suc (Suc Zro)) 
                          `App` 
                          (Var (Suc Zro) 
                           `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp (A.Suc A.Zro)
four = (compose `App` dbl `App` dbl) `App` (ConI 1)

-- Two simple test cases
test :: Bool
test = (case evl four (V.addV ::: Nil) of 
          Rgt (V.ConI 4) -> True
          _              -> False) 

