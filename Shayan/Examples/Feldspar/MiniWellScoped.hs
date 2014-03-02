{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleContexts, DataKinds, PolyKinds, TypeOperators #-}
module Examples.Feldspar.MiniWellScoped where

import Prelude hiding (abs,sum,snd,fst)
import Expression.Feldspar.MiniWellScoped
import Evaluation hiding (Env)
import Evaluation.Feldspar.MiniWellScoped ()
import Singleton.Environment hiding (Env)
import Variable
import ErrorMonad 
import qualified Type.Feldspar as A
import Singleton

type ta :-> tb = A.Arr ta tb

type Env = A.Int :-> (A.Int :-> A.Bol) ':
           A.Bol :-> A.Bol ': 
           A.Int :-> (A.Int :-> A.Int) ':
           A.Int :-> (A.Int :-> A.Int) ':
           A.Int :-> (A.Int :-> A.Int) ': 
           '[]   
           
env :: Trm Env
env = ((==),(not,((+),((*),(min,())))))

type Vector a = (Exp Env A.Int -> a, Exp Env A.Int)

(===) :: Exp Env A.Int -> Exp Env A.Int -> Exp Env A.Bol
e1 === e2 = appV Zro (e1 `Ext` (e2 `Ext` Emp))   

(/==) :: Exp Env A.Int -> Exp Env A.Int -> Exp Env A.Bol
e1 /== e2 =  appV (Suc Zro) (e1 === e2 `Ext` Emp) 

(+.) :: Exp Env A.Int -> Exp Env A.Int -> Exp Env A.Int
e1 +. e2 = appV (Suc (Suc Zro)) (e1 `Ext` (e2 `Ext` Emp))  

(*.) :: Exp Env A.Int -> Exp Env A.Int -> Exp Env A.Int
e1 *. e2 =  appV (Suc (Suc (Suc Zro))) (e1 `Ext` (e2 `Ext` Emp))  

minF :: Exp Env A.Int -> Exp Env A.Int -> Exp Env A.Int
minF e1 e2 = appV (Suc (Suc (Suc (Suc Zro)))) (e1 `Ext` (e2 `Ext` Emp))  

sumv :: Vector (Exp Env A.Int) -> Exp Env A.Int
sumv (ixf,l) = fst $
               Whl ((/== l) . snd) 
               (\s -> Tpl (ixf (snd s) +. (fst s)) (snd s +. ConI 1)) 
               (Tpl (ConI 0) (ConI 0))

mapv :: (a -> b) -> Vector a -> Vector b
mapv f (ixf,l) = (f. ixf,l)

zipWithv :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWithv f (ixf1,l1) (ixf2,l2) = (ixf,minF l1 l2)
  where ixf i = f (ixf1 i) (ixf2 i)

scalarProd :: Vector (Exp Env A.Int) -> Vector (Exp Env A.Int) -> 
              Exp Env A.Int
scalarProd vecA vecB = sumv (zipWithv (*.) vecA vecB)

axpy :: Exp Env A.Int -> Vector (Exp Env A.Int) -> 
        Vector (Exp Env A.Int) -> Vector (Exp Env A.Int)
axpy a x y = zipWithv (+.) (mapv (a*.) x) y

test :: Bool
test = case evl (scalarProd  (id,ConI 2) (((+.) (ConI 1)),ConI 2)) env of 
  Rgt x -> x == 2
  Lft _ -> False
  
