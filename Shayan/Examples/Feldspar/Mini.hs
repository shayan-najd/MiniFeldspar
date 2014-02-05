{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleContexts, NoMonomorphismRestriction #-}
module Examples.Feldspar.Mini where

import Prelude hiding (abs,sum)
import Expression.Feldspar.Mini
import Variable.GADT
import Evaluation as E
import Evaluation.Feldspar.Mini ()
import qualified Value.Feldspar.GADT as V
import Singleton
import Singleton.TypeFeldspar ()
import Type.Feldspar.GADT hiding (Tpl)

-- An example expression doubling the input number                    
dbl :: Exp (Integer -> Integer)
dbl =  undefined -- \ x ->  Var "+" (+)   

-- An example expression composing two types
compose :: Exp ((tb -> tc) -> (ta -> tb) -> (ta -> tc))
compose =  undefined
  
-- An example expression representing the Integer 4
four :: Exp Integer
four = undefined -- (compose `App` dbl `App` dbl) `App` (ConI 1)
 
-- Two simple test cases
test :: Bool
test = evl four () == return 4

type Vector a = (Exp Integer -> a, Exp Integer)

(===) :: Eq a => Exp a -> Exp a -> Exp Bool
(===) = Prm2 "=" (==)

(/==) :: Eq a => Exp a -> Exp a -> Exp Bool
(/==) = Prm2 "/=" (/=)

(+.) :: Num a => Exp a -> Exp a -> Exp a
(+.) = Prm2 "+" (+)

(*.) :: Num a => Exp a -> Exp a -> Exp a
(*.) = Prm2 "*" (*)

minF :: Ord a => Exp a -> Exp a -> Exp a
minF = Prm2 "min" (min)

sumv :: Vector (Exp Integer) -> Exp Integer
sumv (ixf,l) = Fst $
              Whl ((/== l) . Snd) (\s -> Tpl (ixf (Snd s) +. (Fst s)) (Snd s +. ConI 1)) (Tpl (ConI 0) (ConI 0))

mapv :: (a -> b) -> Vector a -> Vector b
mapv f (ixf,l) = (f. ixf,l)

zipWithv :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWithv f (ixf1,l1) (ixf2,l2) = (ixf,minF l1 l2)
  where ixf i = f (ixf1 i) (ixf2 i)

scalarProd :: Vector (Exp Integer) -> Vector (Exp Integer) -> Exp Integer
scalarProd vecA vecB = sumv (zipWithv (*.) vecA vecB)

axpy :: Num a => Exp a -> Vector (Exp a) -> Vector (Exp a) -> Vector (Exp a)
axpy a x y = zipWithv (+.) (mapv (a*.) x) y
