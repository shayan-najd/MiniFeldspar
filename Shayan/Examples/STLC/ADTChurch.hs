module Examples.STLC.ADTChurch where
 
import Expression.STLC.ADTChurch  
import Data.Nat
import qualified Expression.STLC.ADTValue as V
import ErrorMonad
import Evaluation 
import Evaluation.STLC.ADTChurch ()
import qualified Type.STLC as AS
import TypeChecking.STLC.ADTChurch ()
import Inference
import Conversion.Type.STLC ()

-- An example expression doubling the input number
dbl :: Exp AS.Typ
dbl = Abs AS.Int (Var Zro `Add` Var Zro)

-- An example expression composing two types
compose :: AS.Typ -> AS.Typ -> AS.Typ -> Exp AS.Typ
compose ta tb tc = Abs (AS.Arr tb tc) 
                (Abs (AS.Arr ta tb) 
                 (Abs ta
                  (Var (Suc (Suc Zro)) `App` (Var (Suc Zro) `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp AS.Typ
four = (compose AS.Int AS.Int AS.Int `App` dbl `App` dbl) `App` (Con 1)

-- Two simple test cases
test :: Bool
test = (case evl four [] of 
          Rgt (V.Con 4) -> True
          _             -> False) 
       && (typChk four [] == Rgt AS.Int)