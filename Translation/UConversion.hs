{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Converstion where

import UExp
import Exp
import Unsafe.Coerce       
       
data WExp where 
  WrapExp ::  Exp e a -> WExp
 
convert :: UExp -> WExp
convert (UCon i)      =       WrapExp $ Con i
convert (UAdd e1 e2)  = case (convert e1,convert e2) of
 (WrapExp e1',WrapExp e2') -> WrapExp $ Add (unsafeCoerce e1') (unsafeCoerce e2')
convert (UVar uv)     = case convertVar uv of
 WrapVar v                 -> WrapExp $ Var v 
convert (UAbs eb)     = case convert eb of
 WrapExp eb'               -> WrapExp $ Abs (unsafeCoerce eb')
convert (UApp e1 e2)  = case (convert e1,convert e2) of
 (WrapExp e1',WrapExp e2') -> WrapExp $ App (unsafeCoerce e1') (unsafeCoerce e2')
    
data WVar where 
  WrapVar :: Var e a -> WVar 

convertVar :: UVar -> WVar 
convertVar  UZ     = WrapVar Z
convertVar (US uv) = case convertVar uv of
 (WrapVar v)      -> WrapVar $ S v

data WVal where
  WrapVal :: a -> WVal

weval :: WExp -> WVal
weval (WrapExp ex) = WrapVal (eval (unsafeCoerce ex) ()) 
                 -- the unsafeCoerce here is to enforce the assumption that
                 -- environment of "ex" (the first type parameter) is empty
                 -- (i.e. type ())

prop_correctConvert :: UExp -> Bool
prop_correctConvert ue = case (ueval ue emptyUEnv,weval $ convert ue) of
    (I i,WrapVal r) -> i == unsafeCoerce r
    (F f,WrapVal r) -> -- testing the (extensional) equality of the two functions
                       -- of type Int -> Int
                       and [f (I i) == I ((unsafeCoerce r) i) 
                           | i <- [-100..100]]
      
pcheck1 :: Bool
pcheck1 = prop_correctConvert udouble

pcheck2 :: Bool
pcheck2 = prop_correctConvert ufour