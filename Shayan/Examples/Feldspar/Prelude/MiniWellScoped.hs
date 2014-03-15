module Examples.Feldspar.Prelude.MiniWellScoped where

import Prelude ()
import MyPrelude 
 
import Expression.Feldspar.MiniWellScoped
import qualified Expression.Feldspar.GADTValue as V
import qualified Type.Feldspar.ADT             as TFA

import Conversion 
import Conversion.Expression.Feldspar.Evaluation.MiniWellScoped ()

import Environment.Typed
import Variable.Typed

type ta :-> tb = TFA.Arr ta tb

type EnvT = TFA.Int :-> (TFA.Int :-> TFA.Bol) ':
            TFA.Bol :-> TFA.Bol ': 
            TFA.Int :-> (TFA.Int :-> TFA.Int) ':
            TFA.Int :-> (TFA.Int :-> TFA.Int) ':
            TFA.Int :-> (TFA.Int :-> TFA.Int) ': 
            '[]   
           
env :: Env V.Val EnvT
env = Ext (V.Val (==)) 
      (Ext (V.Val not) 
       (Ext (V.Val (+))                  
        (Ext (V.Val (*))                  
         (Ext (V.Val min)                            
          Emp))))
      
type Vector a = (Exp EnvT TFA.Int -> a, Exp EnvT TFA.Int)

(===) :: Exp EnvT TFA.Int -> Exp EnvT TFA.Int -> Exp EnvT TFA.Bol
e1 === e2 = AppV Zro (Ext e1 (Ext e2 Emp))   

(/==) :: Exp EnvT TFA.Int -> Exp EnvT TFA.Int -> Exp EnvT TFA.Bol
e1 /== e2 =  AppV (Suc Zro) (Ext (e1 === e2) Emp) 

(+.) :: Exp EnvT TFA.Int -> Exp EnvT TFA.Int -> Exp EnvT TFA.Int
e1 +. e2 = AppV (Suc (Suc Zro)) (Ext e1 (Ext e2 Emp))  

(*.) :: Exp EnvT TFA.Int -> Exp EnvT TFA.Int -> Exp EnvT TFA.Int
e1 *. e2 =  AppV (Suc (Suc (Suc Zro))) (Ext e1 (Ext e2 Emp))  

minF :: Exp EnvT TFA.Int -> Exp EnvT TFA.Int -> Exp EnvT TFA.Int
minF e1 e2 = AppV (Suc (Suc (Suc (Suc Zro)))) (Ext e1 (Ext e2 Emp))  

sumv :: Vector (Exp EnvT TFA.Int) -> Exp EnvT TFA.Int
sumv (ixf,l) = Fst (
               Whl ((/== l) . Snd) 
               (\ s -> Tpl (ixf (Snd s) +. (Fst s)) (Snd s +. ConI 1)) 
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
  Rgt ((V.Val x) :: V.Val TFA.Int) -> x == 2
  _                                -> False
  
dbl :: Exp EnvT TFA.Int -> Exp EnvT TFA.Int
dbl x = x +. x

four :: Exp EnvT TFA.Int
four = (dbl . dbl) (ConI 1)