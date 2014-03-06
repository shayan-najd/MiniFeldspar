{-# OPTIONS_GHC -Wall #-}
module Examples.Feldspar.GADTTyped where
 
import Expression.Feldspar.GADTTyped 
import Data.Fin
import qualified Data.Nat as N
import qualified Expression.Feldspar.ADTValue as V
import ErrorMonad
import Evaluation 
import Evaluation.Feldspar.GADTTyped ()
import qualified Type.Feldspar as FS
import TypeChecking.Feldspar ()
import Inference
import Conversion.Type.Feldspar ()
import qualified Data.Vector as E

-- An example expression doubling the input number
dbl :: Exp (N.Suc N.Zro) FS.Typ
dbl = Abs (App FS.Int (App FS.Int (Var (Suc Zro)) (Var Zro)) (Var Zro))

-- An example expression composing two types
compose :: FS.Typ -> FS.Typ -> Exp n FS.Typ
compose ta tb = Abs (Abs (Abs 
  (App tb (Var (Suc (Suc Zro))) 
   (App ta (Var (Suc Zro)) (Var Zro)))))
   

-- An example expression representing the Integer 4
four :: Exp (N.Suc N.Zro) FS.Typ
four = App FS.Int 
       (App (FS.Arr FS.Int FS.Int) 
        (App (FS.Arr FS.Int FS.Int) 
         (compose FS.Int FS.Int) dbl) dbl) (ConI 1)

-- Two simple test cases
test :: Bool
test = (case evl four (V.addV E.::: E.Nil) of 
          Rgt (V.ConI 4) -> True
          _              -> False) 
       && (typChk four (FS.Int `FS.Arr` (FS.Int `FS.Arr` FS.Int) E.::: E.Nil)
           == 
           Rgt FS.Int)