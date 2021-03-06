module Examples.Feldspar.Simple.GADTHigherOrder where

import MyPrelude

import Expression.Feldspar.GADTHigherOrder
import Variable.Typed
import Conversion as E
import Expression.Feldspar.Conversions.Evaluation.GADTHigherOrder ()
import qualified Expression.Feldspar.GADTValue as FGV
import Singleton
import Type.Feldspar.GADT
import qualified Type.Feldspar.ADT as TFA
import Environment.Typed

dbl :: Exp (TFA.Arr TFA.Int (TFA.Arr TFA.Int TFA.Int) ': '[])
       (TFA.Arr TFA.Int TFA.Int)
dbl = Abs (\ x -> (App (App (Var Zro) x) x))

compose :: (HasSin Typ ta , HasSin Typ tb , HasSin Typ tc) =>
           Exp r (TFA.Arr (TFA.Arr tb tc) (TFA.Arr (TFA.Arr ta tb)
                                           (TFA.Arr ta tc)))
compose = Abs (\ g -> Abs (\ f -> Abs
                    (\ x -> App g (App f x))))

four :: Exp (TFA.Arr TFA.Int (TFA.Arr TFA.Int TFA.Int) ': '[]) TFA.Int
four = App (App (App compose dbl) dbl) (ConI 1)

test :: Bool
test = case cnv ( four
                , Ext (FGV.Exp (+)
                       :: FGV.Exp (TFA.Arr TFA.Int (TFA.Arr TFA.Int TFA.Int)))
                  Emp) of
  Rgt (FGV.Exp x) -> x == 4
  Lft _           -> False
