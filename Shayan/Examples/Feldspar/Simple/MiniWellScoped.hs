module Examples.Feldspar.Simple.MiniWellScoped where

import MyPrelude 

import Expression.Feldspar.MiniWellScoped
import Environment.Typed
import qualified Expression.Feldspar.GADTValue as FGV
import Variable.Typed
import Conversion
import Conversion.Expression.Feldspar.Evaluation.MiniWellScoped ()
import qualified Type.Feldspar.ADT as A
import Type.Feldspar.ADT  as TFA

type Add = A.Arr A.Int (A.Arr A.Int A.Int)
type EnvAdd = Add ': '[]

(+.) :: Exp EnvAdd A.Int -> Exp EnvAdd A.Int -> Exp EnvAdd A.Int
e1 +. e2 = AppV Zro (Ext e1 (Ext e2 Emp))  

dbl :: Exp EnvAdd A.Int -> Exp EnvAdd A.Int
dbl x = x +. x 

compose :: (Exp r tb -> Exp r tc) -> (Exp r ta -> Exp r tb) 
           -> Exp r ta -> Exp r tc
compose = (.)

four :: Exp EnvAdd A.Int
four = (dbl . dbl) (ConI 1)

test :: Bool
test = case cnv (four 
                , Ext (FGV.Exp (+) 
                       :: FGV.Exp (TFA.Arr TFA.Int (TFA.Arr TFA.Int TFA.Int)))  
                  Emp) of  
  Rgt x -> x FGV.=== FGV.Exp (4 :: Integer)
  Lft _ -> False
