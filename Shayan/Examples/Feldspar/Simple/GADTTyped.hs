module Examples.Feldspar.Simple.GADTTyped where

import MyPrelude

import Expression.Feldspar.GADTTyped
import Variable.Scoped
import qualified Type.Feldspar.ADT as TFA
import qualified Expression.Feldspar.ADTValue as V
import qualified Nat.ADT as NA
import Environment.Scoped
import Conversion
import Expression.Feldspar.Conversions.Evaluation.GADTTyped ()
import TypeChecking.Feldspar ()
import Inference

dbl :: Exp (NA.Suc NA.Zro) TFA.Typ
dbl = Abs (App TFA.Int (App TFA.Int (Var (Suc Zro)) (Var Zro)) (Var Zro))

compose :: TFA.Typ -> TFA.Typ -> Exp n TFA.Typ
compose ta tb = Abs (Abs (Abs
  (App tb (Var (Suc (Suc Zro)))
   (App ta (Var (Suc Zro)) (Var Zro)))))

four :: Exp (NA.Suc NA.Zro) TFA.Typ
four = App TFA.Int
       (App (TFA.Arr TFA.Int TFA.Int)
        (App (TFA.Arr TFA.Int TFA.Int)
         (compose TFA.Int TFA.Int) dbl) dbl) (ConI 1)

test :: Bool
test = (case cnv (four , (Ext V.addV Emp)) of
          Rgt (V.ConI 4) -> True
          _              -> False)
       && (typChk four (Ext (TFA.Arr TFA.Int
                             (TFA.Arr TFA.Int TFA.Int)) Emp)
           ==
           Rgt TFA.Int)