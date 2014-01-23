{-# OPTIONS_GHC -Wall #-}
module Examples.STLC.ADTChurchPolymorphic where
 
import Expression.STLC.ADTChurchPolymorphic  
import Variable.ADT
import qualified Value.STLC.ADT as V
import ErrorMonad
import Evaluation 
import Evaluation.STLC.ADTChurchPolymorphic ()
import qualified Type.STLC.ADTSimple as AS
import TypeChecking.STLC.ADTChurchPolymorphic ()
import TypeChecking 
import Unification.STLC.ADTSimple () 

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
       && (chk four [] == Rgt AS.Int)