{-# OPTIONS_GHC -Wall #-}
module Examples.Feldspar.ADTUntypedNamed where
 
import Expression.Feldspar.ADTUntypedNamed 
import qualified Expression.Feldspar.ADTValue as V
import ErrorMonad
import Evaluation  
import Evaluation.Feldspar.ADTUntypedNamed ()

type Var = String

-- An example expression doubling the input number
dbl :: Exp Var
dbl = Abs "x0" (App (App (Var "add") (Var "x0")) (Var "x0"))

-- An example expression composing two types
compose :: Exp Var
compose = Abs "x2" (Abs "x1" (Abs "x0" (Var "x2" 
                          `App` 
                          (Var "x1" 
                           `App` Var "x0"))))

-- An example expression representing the Integer 4
four :: Exp Var
four = (compose `App` dbl `App` dbl) `App` (ConI 1)

-- Two simple test cases
test :: Bool
test = (case evl four [("add",V.addV)] of 
          Rgt (V.ConI 4) -> True
          _              -> False) 

