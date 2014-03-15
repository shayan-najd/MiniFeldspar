module Examples.Feldspar.ADTUntypedDebruijn where
 
import Prelude ()
import MyPrelude

import Expression.Feldspar.ADTUntypedDebruijn
import Variable.Plain
import qualified Expression.Feldspar.ADTValue as V
import Conversion
import Conversion.Expression.Feldspar.Evaluation.ADTUntypedDebruijn ()
  
dbl :: Exp
dbl = Abs (App (App (Var (Suc Zro)) (Var Zro)) (Var Zro))

compose :: Exp
compose = Abs (Abs (Abs (App (Var (Suc (Suc Zro))) 
                          (App (Var (Suc Zro)) 
                           (Var Zro)))))

four :: Exp
four = App (App (App compose dbl) dbl) (ConI 1)

test :: Bool
test = (case cnv (four , [V.addV]) of 
          Rgt (V.ConI 4) -> True
          _              -> False) 

