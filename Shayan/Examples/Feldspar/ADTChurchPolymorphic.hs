{-# OPTIONS_GHC -Wall #-}
module Examples.Feldspar.ADTChurchPolymorphic where
 
import Expression.Feldspar.ADTChurchPolymorphic  
import Variable.ADT
import qualified Value.Feldspar.ADT as V
import ErrorMonad
import Evaluation 
import Evaluation.Feldspar.ADTChurchPolymorphic ()
import qualified Type.Feldspar.ADTSimple as FS
import TypeChecking.Feldspar.ADTChurchPolymorphic ()
import TypeChecking 
import Unification.Feldspar.ADTSimple () 

-- An example expression doubling the input number
dbl :: Exp FS.Typ
dbl = Abs FS.Int (App (App (Var (Suc Zro)) (Var Zro)) (Var Zro))

-- An example expression composing two types
compose :: FS.Typ -> FS.Typ -> FS.Typ -> Exp FS.Typ
compose ta tb tc = Abs (FS.Arr tb tc) 
                (Abs (FS.Arr ta tb) 
                 (Abs ta
                  (Var (Suc (Suc Zro)) `App` (Var (Suc Zro) `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp FS.Typ
four = (compose FS.Int FS.Int FS.Int `App` dbl `App` dbl) `App` (ConI 1)

-- Two simple test cases
test :: Bool
test = (case evl four [V.addV] of 
          Rgt (V.ConI 4) -> True
          _             -> False) 
       && (chk four [FS.Int `FS.Arr` (FS.Int `FS.Arr` FS.Int)] == Rgt FS.Int)