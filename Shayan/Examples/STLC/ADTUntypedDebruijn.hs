{-# OPTIONS_GHC -Wall #-}
module Examples.STLC.ADTUntypedDebruijn where
 
import Expression.STLC.ADTUntypedDebruijn
import Data.Nat
import qualified Expression.STLC.ADTValue as V
import ErrorMonad
import Evaluation  
import Evaluation.STLC.ADTUntypedDebruijn ()

-- An example expression doubling the input number
dbl :: Exp
dbl = Abs (Var Zro `Add` Var Zro)

-- An example expression composing two types
compose :: Exp
compose = Abs (Abs (Abs  (Var (Suc (Suc Zro)) 
                          `App` 
                          (Var (Suc Zro) 
                           `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp
four = (compose `App` dbl `App` dbl) `App` (Con 1)

-- Two simple test cases
test :: Bool
test = (case evl four [] of 
          Rgt (V.Con 4) -> True
          _             -> False) 

