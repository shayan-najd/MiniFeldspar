module Examples.Feldspar.Prelude.GADTFirstOrder where

import Prelude ()
import MyPrelude 
 
import Expression.Feldspar.GADTFirstOrder
import qualified Expression.Feldspar.GADTValue as FGV
import qualified Type.Feldspar.ADT             as TFA
import qualified Type.Feldspar.GADT            as TFG

import Conversion 
import Conversion.Expression.Feldspar.Evaluation.GADTFirstOrder ()

import Environment.Typed
import Variable.Typed
import Singleton

type ta :-> tb = TFA.Arr ta tb

type EnvT = TFA.Int :-> (TFA.Int :-> TFA.Bol) ':
            TFA.Bol :-> TFA.Bol ': 
            TFA.Int :-> (TFA.Int :-> TFA.Int) ':
            TFA.Int :-> (TFA.Int :-> TFA.Int) ':
            TFA.Int :-> (TFA.Int :-> TFA.Int) ': 
            '[]   
           
env :: Env FGV.Exp EnvT
env = Ext (FGV.Exp (==)) 
      (Ext (FGV.Exp not) 
       (Ext (FGV.Exp (+))                  
        (Ext (FGV.Exp (*))                  
         (Ext (FGV.Exp min)                            
          Emp))))
      
type Vector r a = (Exp r (TFA.Arr TFA.Int a), Exp r TFA.Int)

eqlVar :: Var EnvT (TFA.Int :-> (TFA.Int :-> TFA.Bol))
eqlVar = Zro

notVar :: Var EnvT (TFA.Bol :-> TFA.Bol)
notVar = Suc Zro

addVar :: Var EnvT (TFA.Int :-> (TFA.Int :-> TFA.Int))
addVar = Suc (Suc Zro)

mulVar :: Var EnvT (TFA.Int :-> (TFA.Int :-> TFA.Int))
mulVar = Suc (Suc (Suc Zro))
  
minVar :: Var EnvT (TFA.Int :-> (TFA.Int :-> TFA.Int))
minVar = Suc (Suc (Suc (Suc Zro)))

var :: (?env :: Env tf r') =>
       Var r t -> Exp (Add r' r) t
var = Var . add
 
(===) :: (?env :: Env tf r) =>
         Exp (Add r EnvT) TFA.Int -> Exp (Add r EnvT) TFA.Int -> 
         Exp (Add r EnvT) TFA.Bol
e1 === e2 = App (App (var eqlVar) e1) e2

(/==) :: (?env :: Env tf r) => 
         Exp (Add r EnvT) TFA.Int -> Exp (Add r EnvT) TFA.Int -> 
         Exp (Add r EnvT) TFA.Bol
e1 /== e2 = App (var notVar) (e1 === e2)

(+.) :: (?env :: Env tf r) =>
        Exp (Add r EnvT) TFA.Int -> Exp (Add r EnvT) TFA.Int -> 
        Exp (Add r EnvT) TFA.Int
e1 +. e2 = App (App (var addVar) e1) e2

(*.) :: (?env :: Env tf r) =>
        Exp (Add r EnvT) TFA.Int -> Exp (Add r EnvT) TFA.Int -> 
        Exp (Add r EnvT) TFA.Int
e1 *. e2 =  App (App (var mulVar) e1) e2

minF :: (?env :: Env tf r) =>
        Exp (Add r EnvT) TFA.Int -> Exp (Add r EnvT) TFA.Int -> 
        Exp (Add r EnvT) TFA.Int
minF e1 e2 = App (App (var minVar) e1) e2
 
sumv :: (?env :: Env T r) =>
        Vector (Add r EnvT) TFA.Int -> Exp (Add r EnvT) TFA.Int
sumv (ixf,l) =  let r = ?env 
                in Fst (Whl 
                        (let ?env = Ext T r 
                         in  (Snd (Var Zro)) /== (mapVar Suc l)) 
                        (let ?env = Ext T r 
                         in  Tpl ((App (mapVar Suc ixf) (Snd (Var Zro))) 
                                  +. (Fst (Var Zro))) 
                             ((Snd (Var Zro)) +. (ConI 1))) 
                        (Tpl (ConI 0) (ConI 0)))

mapv :: (HasSin TFG.Typ a , ?env :: Env T r) => 
        Exp (Add r EnvT) (TFA.Arr a b) -> Vector (Add r EnvT) a -> 
        Vector (Add r EnvT) b
mapv f (ixf , l) = (Abs (App (mapVar Suc f) 
                         (App (mapVar Suc ixf) (Var Zro))) , l)
 
zipWithv :: (HasSin TFG.Typ a , HasSin TFG.Typ b , ?env :: Env T r) =>
            Exp (Add r EnvT) (TFA.Arr a (TFA.Arr b c)) -> 
            Vector (Add r EnvT) a -> 
            Vector (Add r EnvT) b -> Vector (Add r EnvT) c
zipWithv f (ixf1,l1) (ixf2,l2) = (ixf , minF l1 l2)
  where ixf  = Abs (App (App (sucAll f) 
                         (App (sucAll ixf1) (Var Zro))) 
                    (App (sucAll ixf2) (Var Zro)))

scalarProd :: (?env :: Env T r) => 
              Vector (Add r EnvT) TFA.Int -> Vector (Add r EnvT) TFA.Int -> 
              Exp (Add r EnvT) TFA.Int
scalarProd vecA vecB = sumv (zipWithv (var mulVar) vecA vecB)


axpy :: (?env :: Env T r) => 
        Exp (Add r EnvT) TFA.Int -> Vector (Add r EnvT) TFA.Int -> 
        Vector (Add r EnvT) TFA.Int -> Vector (Add r EnvT) TFA.Int
axpy a x y = let r = ?env in 
  zipWithv (var addVar) (mapv (Abs (let ?env = Ext T r 
                                    in (sucAll a)  *. (Var Zro))) x) y
 
test :: Bool
test = let ?env = Emp in 
  case curry cnv (scalarProd  (Abs (Var Zro) , ConI 2) 
                  ((Abs (let ?env = Ext T Emp 
                         in (Var Zro) +. (ConI 1))) , ConI 2)) env of 
    Rgt ((FGV.Exp x) :: FGV.Exp TFA.Int) -> x == 2
    _                                -> False