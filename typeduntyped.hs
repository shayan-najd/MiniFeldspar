{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

-- Typed and untyped deBruijn

module TypedAndUntyped where

import Unsafe.Coerce
import Data.Data

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

data WExp where 
  Wrap ::  Exp e a -> WExp
 
convert :: UExp -> WExp
convert (UCon i)      = Wrap $ Con i
convert (UAdd e1 e2)  = case (convert e1,convert e2) of
 (Wrap e1',Wrap e2') -> Wrap $ Add (unsafeCoerce e1') (unsafeCoerce e2')
convert (UVar uv)     = case convertVar uv of
 WrapVar v           -> Wrap $ Var v 
convert (UAbs eb)     = case convert eb of
 Wrap eb'            -> Wrap $ Abs (unsafeCoerce eb')
convert (UApp e1 e2)  = case (convert e1,convert e2) of
 (Wrap e1',Wrap e2') -> Wrap $ App (unsafeCoerce e1') (unsafeCoerce e2')
    
data WVar where 
  WrapVar :: Var e a -> WVar 

convertVar :: UVar -> WVar 
convertVar  UZ     = WrapVar Z
convertVar (US uv) = case convertVar uv of
 (WrapVar v)      -> WrapVar $ S v

prop_correctConvert :: UExp -> Bool
prop_correctConvert ue = case convert ue of
  Wrap e -> case ueval ue [] of 
    I i -> i       == eval (unsafeCoerce e) ()
    F f -> -- testing the (extensional) equality of the two functions 
           -- of type Int -> Int
      and [f (I i) == I (eval (unsafeCoerce e) () i) 
          | i <- [-100..100]]
  
pcheck :: Bool
pcheck = prop_correctConvert udouble

data WVal where
  WrapVal :: a -> WVal

weval :: WExp -> WVal
weval (Wrap ex) = WrapVal (eval (unsafeCoerce ex) ()) 
                 -- the unsafeCoerce here is to enforce the assumption that
                 -- environment of "ex" (the first type parameter) is empty
                 -- (i.e. type ())

prop_correctConvert' :: UExp -> Bool
prop_correctConvert' ue = case (ueval ue [],weval $ convert ue) of
    (I i,WrapVal r) -> i       == unsafeCoerce r
    (F f,WrapVal r) -> -- testing the (extensional) equality of the two functions
               -- of type Int -> Int
      and [f (I i) == I ((unsafeCoerce r) i) 
          | i <- [-100..100]]
      
pcheck' :: Bool
pcheck' = prop_correctConvert' udouble


