{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Exp where

type Val a = a

type Env e = e

type EmptyEnv = Env ()

emptyEnv :: EmptyEnv
emptyEnv = ()

data Var e a where
  Z :: Var (e,a) a
  S :: Var e a -> Var (e,b) a

data Exp e a where
  Con :: Int -> Exp e Int
  Add :: Exp e Int -> Exp e Int -> Exp e Int
  Var :: Var e a -> Exp e a
  Abs :: Exp (e,a) b -> Exp e (a -> b)
  App :: Exp e (a -> b) -> Exp e a -> Exp e b

fetch :: Var e a -> Env e -> Val a
fetch Z     (_,a)  =  a
fetch (S x) (e,_)  =  fetch x e

eval :: Exp e a -> Env e -> Val a
eval (Con n)   _  =  n
eval (Add m n) e  =  eval m e + eval n e
eval (Var x)   e  =  fetch x e
eval (Abs b)   e  =  \a -> eval b (e,a)
eval (App f a) e  =  eval f e (eval a e)

double :: Exp EmptyEnv (Int -> Int)
double = Abs (Var Z `Add` Var Z)

compose :: Exp EmptyEnv ((b -> c) -> (a -> b) -> (a -> c))
compose = Abs (Abs (Abs (Var (S (S Z)) `App` (Var (S Z) `App` Var Z))))

four :: Exp EmptyEnv Int
four = (compose `App` double `App` double) `App` (Con 1)

check :: Bool
check = (eval four emptyEnv == 4)
