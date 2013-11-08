{-# LANGUAGE Rank2Types, GADTs, ScopedTypeVariables #-}

-- Typed and untyped deBruijn

module TypedAndUntyped where

data Var e a where
  Z :: Var (e,a) a
  S :: Var e a -> Var (e,b) a

data Exp e a where
  Con :: Int -> Exp e Int
  Add :: Exp e Int -> Exp e Int -> Exp e Int
  Var :: Var e a -> Exp e a
  Abs :: Exp (e,a) b -> Exp e (a -> b)
  App :: Exp e (a -> b) -> Exp e a -> Exp e b

fetch :: Var e a -> e -> a
fetch Z (e,a)      =  a
fetch (S x) (e,b)  =  fetch x e

eval :: Exp e a -> e -> a
eval (Con n) e       =  n
eval (Add m n) e     =  eval m e + eval n e
eval (Var x) e       =  fetch x e
eval (Abs b) e       =  \a -> eval b (e,a)
eval (App f a) e     =  eval f e (eval a e)

double :: Exp () (Int -> Int)
double = Abs (Var Z `Add` Var Z)

compose :: Exp () ((b -> c) -> (a -> b) -> (a -> c))
compose = Abs (Abs (Abs (Var (S (S Z)) `App` (Var (S Z) `App` Var Z))))

four :: Exp () Int
four = (compose `App` double `App` double) `App` (Con 1)

check = (eval four () == 4)

-- Untyped deBruijn

data Val where
  I :: Int -> Val
  F :: (Val -> Val) -> Val

instance Show Val where
  show (I n)  =  show n

instance Eq Val where
  I m == I n  =  m == n

data UVar where
  UZ :: UVar
  US :: UVar -> UVar

data UExp where
  UCon :: Int -> UExp
  UAdd :: UExp -> UExp -> UExp
  UVar :: UVar -> UExp
  UAbs :: UExp -> UExp
  UApp :: UExp -> UExp -> UExp

ufetch :: UVar -> [Val] -> Val
ufetch UZ (a:e)      =  a
ufetch (US x) (b:e)  =  ufetch x e

ueval :: UExp -> [Val] -> Val
ueval (UCon n) e       =  I n
ueval (UAdd m n) e     =  ueval m e `add` ueval n e
                       where I m `add` I n = I (m+n)
ueval (UVar x) e       =  ufetch x e
ueval (UAbs b) e       =  F (\a -> ueval b (a:e))
ueval (UApp f a) e     =  ueval f e `app` ueval a e
                       where F f `app` a = f a

udouble :: UExp
udouble = UAbs (UVar UZ `UAdd` UVar UZ)

ucompose :: UExp
ucompose = UAbs (UAbs (UAbs (UVar (US (US UZ)) `UApp` (UVar (US UZ) `UApp` UVar UZ))))

ufour :: UExp
ufour = (ucompose `UApp` udouble `UApp` udouble) `UApp` (UCon 1)

ucheck = (ueval ufour [] == I 4)

