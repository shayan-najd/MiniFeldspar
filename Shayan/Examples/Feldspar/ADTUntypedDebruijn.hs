{-# OPTIONS_GHC -Wall #-}
module Examples.Feldspar.ADTUntypedDebruijn where
 
import Expression.Feldspar.ADTUntypedDebruijn
import Variable.ADT
import qualified Value.Feldspar.ADT as V
import ErrorMonad
import Evaluation  
import Evaluation.Feldspar.ADTUntypedDebruijn ()
 
-- An example expression doubling the input number
dbl :: Exp
dbl = Abs (App (App (Var (Suc Zro)) (Var Zro)) (Var Zro))

-- An example expression composing two types
compose :: Exp
compose = Abs (Abs (Abs  (Var (Suc (Suc Zro)) 
                          `App` 
                          (Var (Suc Zro) 
                           `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp
four = (compose `App` dbl `App` dbl) `App` (ConI 1)

-- Two simple test cases
test :: Bool
test = (case evl four [V.addV] of 
          Rgt (V.ConI 4) -> True
          _              -> False) 

