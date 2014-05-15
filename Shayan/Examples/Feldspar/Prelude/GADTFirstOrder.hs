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
import Examples.Feldspar.Prelude.Environment       

type Vector r a = (Exp r (TFA.Arr TFA.Int a), Exp r TFA.Int)
 
var :: (?env :: Env tf r') =>
       Var r t -> Exp (Add r' r) t
var = Var . add
 
(===) :: (?env :: Env tf r) =>
         Exp (Add r Prelude) TFA.Int -> Exp (Add r Prelude) TFA.Int -> 
         Exp (Add r Prelude) TFA.Bol
e1 === e2 = App (App (var eqlIntVar) e1) e2

(/==) :: (?env :: Env tf r) => 
         Exp (Add r Prelude) TFA.Int -> Exp (Add r Prelude) TFA.Int -> 
         Exp (Add r Prelude) TFA.Bol
e1 /== e2 = Cnd (e1 === e2) (ConB False) (ConB True)  
            --2App (var notVar) (e1 === e2)

(+.) :: (?env :: Env tf r) =>
        Exp (Add r Prelude) TFA.Int -> Exp (Add r Prelude) TFA.Int -> 
        Exp (Add r Prelude) TFA.Int
e1 +. e2 = App (App (var addIntVar) e1) e2

(<.) :: (?env :: Env tf r) =>
        Exp (Add r Prelude) TFA.Int -> Exp (Add r Prelude) TFA.Int -> 
        Exp (Add r Prelude) TFA.Bol
e1 <. e2 = App (App (var ltdIntVar) e1) e2

(*.) :: (?env :: Env tf r) =>
        Exp (Add r Prelude) TFA.Int -> Exp (Add r Prelude) TFA.Int -> 
        Exp (Add r Prelude) TFA.Int
e1 *. e2 =  App (App (var mulIntVar) e1) e2

minF :: (?env :: Env tf r) =>
        Exp (Add r Prelude) TFA.Int -> Exp (Add r Prelude) TFA.Int -> 
        Exp (Add r Prelude) TFA.Int
minF e1 e2 = Cnd (e1 <. e2) e1 e2 
 
sumv :: (?env :: Env T r) =>
        Vector (Add r Prelude) TFA.Int -> Exp (Add r Prelude) TFA.Int
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
        Exp (Add r Prelude) (TFA.Arr a b) -> Vector (Add r Prelude) a -> 
        Vector (Add r Prelude) b
mapv f (ixf , l) = (Abs (App (mapVar Suc f) 
                         (App (mapVar Suc ixf) (Var Zro))) , l)
 
zipWithv :: (HasSin TFG.Typ a , HasSin TFG.Typ b , ?env :: Env T r) =>
            Exp (Add r Prelude) (TFA.Arr a (TFA.Arr b c)) -> 
            Vector (Add r Prelude) a -> 
            Vector (Add r Prelude) b -> Vector (Add r Prelude) c
zipWithv f (ixf1,l1) (ixf2,l2) = (ixf , minF l1 l2)
  where ixf  = Abs (App (App (sucAll f) 
                         (App (sucAll ixf1) (Var Zro))) 
                    (App (sucAll ixf2) (Var Zro)))

scalarProd :: (?env :: Env T r) => 
              Vector (Add r Prelude) TFA.Int -> Vector (Add r Prelude) TFA.Int ->
              Exp (Add r Prelude) TFA.Int
scalarProd vecA vecB = sumv (zipWithv (var mulIntVar) vecA vecB)


axpy :: (?env :: Env T r) => 
        Exp (Add r Prelude) TFA.Int -> Vector (Add r Prelude) TFA.Int -> 
        Vector (Add r Prelude) TFA.Int -> Vector (Add r Prelude) TFA.Int
axpy a x y = let r = ?env in 
  zipWithv (var addIntVar) (mapv (Abs (let ?env = Ext T r 
                                    in (sucAll a)  *. (Var Zro))) x) y
 
test :: Bool
test = let ?env = Emp in 
  case curry cnv (scalarProd  (Abs (Var Zro) , ConI 2) 
                  ((Abs (let ?env = Ext T Emp 
                         in (Var Zro) +. (ConI 1))) , ConI 2)) etFGV of 
    Rgt ((FGV.Exp x) :: FGV.Exp TFA.Int) -> x == 2
    _                                -> False