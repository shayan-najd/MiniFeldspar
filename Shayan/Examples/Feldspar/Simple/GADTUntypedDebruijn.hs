module Examples.Feldspar.Simple.GADTUntypedDebruijn where

import MyPrelude

import Expression.Feldspar.GADTUntypedDebruijn
import Variable.Scoped
import qualified Nat.ADT                      as NA
import qualified Expression.Feldspar.ADTValue as V
import Conversion
import Conversion.Expression.Feldspar.Evaluation.GADTUntypedDebruijn ()
import Environment.Scoped

dbl :: Exp (NA.Suc NA.Zro)
dbl = Abs (App (App (Var (Suc Zro)) (Var Zro)) (Var Zro))

compose :: Exp (NA.Suc NA.Zro)
compose = Abs (Abs (Abs (App (Var (Suc (Suc Zro))) (App (Var (Suc Zro))
                                                    (Var Zro)))))

four :: Exp (NA.Suc NA.Zro)
four = App (App (App compose  dbl) dbl) (ConI 1)

test :: Bool
test = (case cnv (four , Ext V.addV Emp) of
          Rgt (V.ConI 4) -> True
          _              -> False)

