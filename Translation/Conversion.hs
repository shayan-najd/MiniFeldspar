{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, TypeOperators #-}
module Converstion where

import UExp
import Exp
import Control.Monad ((<=<))
 
-- Singleton for Types
data SType a where  
  SInt     :: SType Int 
  (:->)    :: SType t1 -> SType t2 -> SType (t1 -> t2) 
  
-- Singleton for Environments
data SEnv a where
  Empty :: SEnv ()
  (:::) :: SType t -> SEnv e -> SEnv (e,t)

data Equal a b = a ~ b => Refl  

-- Equality for SType with its proof
(=:=) :: SType a -> SType b -> Maybe (Equal a b)
SInt        =:= SInt           =  return Refl
(t1 :-> t2) =:= (t1' :-> t2')  =  do  
  Refl <- t1 =:= t1' 
  Refl <- t2 =:= t2'
  return Refl
_           =:= _              = fail ""
 
-- Wrapper for Exp     
data WExp n where 
  WrapExp :: Exp n t -> SType t -> WExp n
  
--
convert :: SEnv n -> UExp -> Maybe (WExp n)
convert _ (UCon i)       =  
  return $ WrapExp (Con i) SInt
convert n (UAdd ue1 ue2) =  do 
  WrapExp e1 SInt        <- convert n ue1
  WrapExp e2 SInt        <- convert n ue2 
  return $ WrapExp (Add e1 e2) SInt
convert n (UApp ue1 ue2) =  do
  WrapExp e1 (ta :-> tb) <- convert n ue1
  WrapExp e2 ta'         <- convert n ue2 
  Refl <- ta =:= ta'
  return $ WrapExp (App e1 e2) tb
convert n (UVar uv)      =  do
  WrapVar v t            <- convertVar n uv
  return $ WrapExp (Var v) t
convert n (UAbs ueb)     =  do
  let ta = SInt -- <-- WARNING: This is a hack working if the language
                --              is first-order with Int being the only
                --              base type.
  WrapExp eb tb          <- convert (ta ::: n) ueb 
  return $ WrapExp (Abs eb) (ta :-> tb) 

data WVar n  where 
  WrapVar :: Var n t -> SType t -> WVar n 
 
convertVar :: SEnv n -> UVar -> Maybe (WVar n) 
convertVar (t  ::: _) UZ       =  do 
  return $ WrapVar Z     t
convertVar (_  ::: n) (US uv)  =  do 
  WrapVar v t <- convertVar n uv
  return $ WrapVar (S v) t
convertVar _          _        =  fail ""    

data WVal where
  WrapVal :: t -> SType t -> WVal

weval :: Env n -> WExp n -> Maybe WVal
weval n (WrapExp e t) = do 
  return $ WrapVal (eval e n) t 
                 
prop_correctConvert :: UExp -> Maybe Bool
prop_correctConvert ue = case ueval ue emptyUEnv of 
  I i -> do 
    WrapVal r t <- (weval () <=< convert Empty) ue
    Refl <- t =:= SInt
    return $ i == r
  F f -> do
    WrapVal r t <- (weval () <=< convert Empty) ue
    Refl <- t =:= (SInt :-> SInt)
    return $ and [f (I i) == I (r i) 
                 | i <- [-100..100]]

pcheck1 :: Bool
pcheck1 = prop_correctConvert udouble == Just True

pcheck2 :: Bool
pcheck2 = prop_correctConvert ufour   == Just True

