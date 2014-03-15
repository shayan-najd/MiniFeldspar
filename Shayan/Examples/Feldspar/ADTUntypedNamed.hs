module Examples.Feldspar.ADTUntypedNamed where
 
import Prelude ()
import MyPrelude

import Expression.Feldspar.ADTUntypedNamed 
import qualified Expression.Feldspar.ADTValue as V
import Conversion
import Conversion.Expression.Feldspar.Evaluation.ADTUntypedNamed ()
import qualified Language.Haskell.TH.Syntax as TH

type Var = TH.Name

x0 :: Var
x0 = TH.mkName  "x0"

x1 :: Var
x1 = TH.mkName  "x1"

x2 :: Var
x2 = TH.mkName  "x2"

dbl :: Exp Var
dbl = Abs x0 (App (App (Var '(+)) (Var x0)) (Var x0))
                        
compose :: Exp Var
compose = Abs x2 (Abs x1 (Abs x0 (App (Var x2) (App (Var x1) (Var x0)))))

four :: Exp Var
four = App (App (App compose dbl) dbl) (ConI 1)

test :: Bool
test = (case cnv (four , [('(+) , V.addV)]) of 
          Rgt (V.ConI 4) -> True
          _              -> False) 

