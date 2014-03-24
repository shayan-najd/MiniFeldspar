module Examples.Feldspar.Prelude.GADTHigherOrder where

import Prelude ()
import MyPrelude 
 
import Expression.Feldspar.GADTHigherOrder
import qualified Expression.Feldspar.GADTValue as FGV
import qualified Type.Feldspar.ADT             as TFA

import Conversion 
import Conversion.Expression.Feldspar.Evaluation.GADTHigherOrder ()

import Environment.Typed
import Variable.Typed

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
      
type Vector a = (Exp EnvT TFA.Int -> a, Exp EnvT TFA.Int)

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

(===) :: Exp EnvT TFA.Int -> Exp EnvT TFA.Int -> Exp EnvT TFA.Bol
e1 === e2 = App (App (Var eqlVar) e1) e2

(/==) :: Exp EnvT TFA.Int -> Exp EnvT TFA.Int -> Exp EnvT TFA.Bol
e1 /== e2 =  App (Var notVar) (e1 === e2)

(+.) :: Exp EnvT TFA.Int -> Exp EnvT TFA.Int -> Exp EnvT TFA.Int
e1 +. e2 = App (App (Var addVar) e1) e2

(*.) :: Exp EnvT TFA.Int -> Exp EnvT TFA.Int -> Exp EnvT TFA.Int
e1 *. e2 =  App (App (Var mulVar) e1) e2

minF :: Exp EnvT TFA.Int -> Exp EnvT TFA.Int -> Exp EnvT TFA.Int
minF e1 e2 = App (App (Var minVar) e1) e2

sumv :: Vector (Exp EnvT TFA.Int) -> Exp EnvT TFA.Int
sumv (ixf,l) = Fst (
               Whl ((/== l) . Snd) 
               (\ s -> Tpl ((ixf (Snd s)) +. (Fst s)) ((Snd s) +. ConI 1)) 
               (Tpl (ConI 0) (ConI 0)))

mapv :: (a -> b) -> Vector a -> Vector b
mapv f (ixf,l) = (f. ixf,l)

zipWithv :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWithv f (ixf1,l1) (ixf2,l2) = (ixf,minF l1 l2)
  where ixf i = f (ixf1 i) (ixf2 i)

scalarProd :: Vector (Exp EnvT TFA.Int) -> Vector (Exp EnvT TFA.Int) -> 
              Exp EnvT TFA.Int
scalarProd vecA vecB = sumv (zipWithv (*.) vecA vecB)

axpy :: Exp EnvT TFA.Int -> Vector (Exp EnvT TFA.Int) -> 
        Vector (Exp EnvT TFA.Int) -> Vector (Exp EnvT TFA.Int)
axpy a x y = zipWithv (+.) (mapv (a*.) x) y

test :: Bool
test = case curry cnv (scalarProd  (id,ConI 2) (((+.) (ConI 1)),ConI 2)) env of 
  Rgt ((FGV.Exp x) :: FGV.Exp TFA.Int) -> x == 2
  _                                    -> False