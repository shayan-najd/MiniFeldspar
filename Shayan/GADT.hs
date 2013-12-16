{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module GADT where

import Expression.GADT
import Type.GADT
import Variable.GADT
import Environment.GADT

-- Evaluation of expressions under specific environment of values 
evl :: Exp e t -> e -> t
evl (Con i)     _ = i
evl (Var x)     r = get x r
evl (Abs _  eb) r = \ va -> evl eb (va , r)
evl (App ef ea) r = evl ef r $ evl ea r
evl (Add el er) r = evl el r + evl er r

-- Typechecking and returning the type, if successful
chk :: Exp e t -> Env e -> Typ t
chk (Con _)     _ = Int
chk (Var x)     r = gets x r
chk (Abs ta eb) r = ta `Arr` chk eb (ta `Ext` r)
chk (App ef _ ) r = case chk ef r of 
  Arr _ tb -> tb
chk (Add _  _ ) _ = Int 
 
-- An example expression doubling the input number                    
dbl :: Exp () (Int -> Int)
dbl = Abs Int (Var Zro `Add` Var Zro)

-- An example expression composing two types
compose :: Typ ta -> Typ tb -> Typ tc -> 
           Exp () ((tb -> tc) -> (ta -> tb) -> (ta -> tc))
compose ta tb tc = Abs (tb `Arr` tc) (Abs (ta `Arr` tb) (Abs ta
                  (Var (Suc (Suc Zro)) `App` (Var (Suc Zro) `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp () Int
four = (compose Int Int Int `App` dbl `App` dbl) `App` (Con 1)
 
-- Two simple test cases
test :: Bool
test = (evl four () == 4) 
       && (case chk four Emp of 
             Int -> True)
