{-# OPTIONS_GHC -Wall #-}
module Examples.STLC.ADTExplicit where

import Expression.STLC.ADTExplicit
import Variable.ADT
import qualified Value.STLC.ADT as V
import ErrorMonad
import Evaluation
import Evaluation.STLC.ADTExplicit ()
import qualified Type.STLC.ADTSimple as AS
import TypeChecking.STLC.ADTExplicit ()
import TypeChecking
import Unification.STLC.ADTSimple ()

-- An example expression doubling the input number
dbl :: Exp AS.Typ
dbl = Abs (AS.Int `AS.Arr` AS.Int) (Add AS.Int (Var AS.Int Zro) (Var AS.Int Zro))

-- An example expression composing two types
compose :: AS.Typ -> AS.Typ -> AS.Typ -> Exp AS.Typ
compose ta tb tc = Abs (AS.Arr (AS.Arr tb tc) 
                        (AS.Arr (AS.Arr ta tb) 
                         (AS.Arr ta tc))) 
                (Abs (AS.Arr (AS.Arr ta tb) (AS.Arr ta tc))
                 (Abs (AS.Arr ta tc)
                  (App tc (Var (AS.Arr tb tc) (Suc (Suc Zro))) 
                   (App tb (Var (AS.Arr ta tb) (Suc Zro)) (Var ta Zro)))))

-- An example expression representing the Integer 4
four :: Exp AS.Typ
four = App AS.Int 
       (App (AS.Arr AS.Int AS.Int) 
        (App (AS.Arr (AS.Arr AS.Int AS.Int) (AS.Arr AS.Int AS.Int)) 
         (compose AS.Int AS.Int AS.Int)  dbl) dbl) (Con AS.Int 1)
 
-- Two simple test cases
test :: Bool
test = (case evl four [] of 
          Rgt (V.Con 4) -> True
          _             -> False) 
       && ((chk four [] :: ErrM AS.Typ) == Rgt AS.Int)