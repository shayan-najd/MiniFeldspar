module Examples.Feldspar.Prelude.GADTHigherOrder where

import Prelude ()
import MyPrelude 
 
import Expression.Feldspar.GADTHigherOrder
import qualified Expression.Feldspar.GADTValue as FGV
import qualified Type.Feldspar.ADT             as TFA

import Conversion 
import Conversion.Expression.Feldspar.Evaluation.GADTHigherOrder ()

import Examples.Feldspar.Prelude.Environment

type Vector a = (Exp Prelude TFA.Int -> a, Exp Prelude TFA.Int)

(===) :: Exp Prelude TFA.Int -> Exp Prelude TFA.Int -> Exp Prelude TFA.Bol
e1 === e2 = App (App (Var eqlIntVar) e1) e2

(/==) :: Exp Prelude TFA.Int -> Exp Prelude TFA.Int -> Exp Prelude TFA.Bol
e1 /== e2 =  Cnd (e1 === e2) (ConB False) (ConB True) 

(+.) :: Exp Prelude TFA.Int -> Exp Prelude TFA.Int -> Exp Prelude TFA.Int
e1 +. e2 = App (App (Var addIntVar) e1) e2

(*.) :: Exp Prelude TFA.Int -> Exp Prelude TFA.Int -> Exp Prelude TFA.Int
e1 *. e2 =  App (App (Var mulIntVar) e1) e2

(<.) :: Exp Prelude TFA.Int -> Exp Prelude TFA.Int -> Exp Prelude TFA.Bol
e1 <. e2 =  App (App (Var ltdIntVar) e1) e2

minF :: Exp Prelude TFA.Int -> Exp Prelude TFA.Int -> Exp Prelude TFA.Int
minF e1 e2 = Cnd (e1 <. e2) e1 e2

sumv :: Vector (Exp Prelude TFA.Int) -> Exp Prelude TFA.Int
sumv (ixf,l) = Fst (
               Whl ((/== l) . Snd) 
               (\ s -> Tpl ((ixf (Snd s)) +. (Fst s)) ((Snd s) +. ConI 1)) 
               (Tpl (ConI 0) (ConI 0)))

mapv :: (a -> b) -> Vector a -> Vector b
mapv f (ixf,l) = (f. ixf,l)

zipWithv :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWithv f (ixf1,l1) (ixf2,l2) = (ixf,minF l1 l2)
  where ixf i = f (ixf1 i) (ixf2 i)

scalarProd :: Vector (Exp Prelude TFA.Int) -> Vector (Exp Prelude TFA.Int) -> 
              Exp Prelude TFA.Int
scalarProd vecA vecB = sumv (zipWithv (*.) vecA vecB)

axpy :: Exp Prelude TFA.Int -> Vector (Exp Prelude TFA.Int) -> 
        Vector (Exp Prelude TFA.Int) -> Vector (Exp Prelude TFA.Int)
axpy a x y = zipWithv (+.) (mapv (a*.) x) y

test :: Bool
test = case curry cnv (scalarProd  (id,ConI 2) (((+.) (ConI 1)),ConI 2)) etFGV of
  Rgt ((FGV.Exp x) :: FGV.Exp TFA.Int) -> x == 2
  _                                    -> False