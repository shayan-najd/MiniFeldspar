{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module GADT where

-- GADT representation (Debruijn indices) of the simply-typed lambda calculus 
-- expressions with Integer constants and a built-in addition operator
data Exp e t where
  Con :: Int -> Exp e Int
  Add :: Exp e Int -> Exp e Int -> Exp e Int
  Var :: Var e t -> Exp e t
  Abs :: Typ ta -> Exp (ta , e) tb -> Exp e (ta -> tb)
  App :: Exp e (ta -> tb) -> Exp e ta -> Exp e tb

-- Variables
data Var e t where
  Zro :: Var (t , e) t
  Suc :: Var e tp -> Var (ts , e) tp

-- Types (Singleton)
data Typ t where
  Int :: Typ Int
  Arr :: Typ ta -> Typ tb -> Typ (ta -> tb)

-- Environment (Singleton)
data Env e where
  Emp :: Env ()
  Ext :: Typ t -> Env e -> Env (t , e)

-- Extraction of values from environment
get :: Var e t -> e -> t
get Zro     (x , _ ) = x
get (Suc n) (_ , xs) = get n xs

-- Extraction of values from environment with singletons
gets :: Var e t -> Env e -> Typ t
gets Zro     (x `Ext` _ ) = x
gets (Suc n) (_ `Ext` xs) = gets n xs
gets _       Emp          = error "Impossible!" 
                            -- the redundant pattern checker cannot guess that
                            -- and instance of Var never lets the environment to
                            -- to be empty.

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
