module Examples.Feldspar.GADTFirstOrder where

import Prelude ()
import MyPrelude

import Expression.Feldspar.GADTFirstOrder
import Variable.Typed
import Conversion 
import Conversion.Expression.Feldspar.Evaluation.GADTFirstOrder ()
import qualified Expression.Feldspar.GADTValue as FGV
import Singleton
import Environment.Typed

import qualified Type.Feldspar.ADT  as TFA
import qualified Type.Feldspar.GADT as TFG

dbl :: Exp (TFA.Arr TFA.Int (TFA.Arr TFA.Int TFA.Int) ': '[]) 
       (TFA.Arr TFA.Int TFA.Int)
dbl = Abs (App (App (Var (Suc Zro)) (Var Zro)) (Var Zro))

compose :: (HasSin TFG.Typ ta , HasSin TFG.Typ tb , HasSin TFG.Typ tc) =>
           Exp r (TFA.Arr (TFA.Arr tb tc) (TFA.Arr (TFA.Arr ta tb) 
                   (TFA.Arr ta tc)))
compose = Abs (Abs (Abs 
                    (App (Var (Suc (Suc Zro))) 
                     (App (Var (Suc Zro)) (Var Zro)))))
          
four :: Exp (TFA.Arr TFA.Int (TFA.Arr TFA.Int TFA.Int) ': '[]) TFA.Int
four = App (App (App compose dbl) dbl) (ConI 1)
 
test :: Bool
test = case cnv (four , Ext FGV.addV Emp) of 
  Rgt x -> x FGV.=== FGV.Exp 4
  Lft _ -> False
