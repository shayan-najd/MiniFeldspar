{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, TypeOperators #-}
module Converstion where

import UExp
import Exp
import Control.Monad ((<=<))
        
data Equal a b = a ~ b => Refl 

class SEq s where
  (=:=) :: s a -> s b -> Maybe (Equal a b)

-- Singleton for Types
data SType a where  
  SInt     :: SType Int 
  (:->)    :: SType t1 -> SType t2 -> SType (t1 -> t2) 
  AnySType :: SType a 
  
instance SEq SType where
  SInt        =:= SInt           =  return Refl
  (t1 :-> t2) =:= (t1' :-> t2')  =  do  
    Refl <- t1 =:= t1' 
    Refl <- t2 =:= t2'
    return Refl
  _           =:= _              = fail ""
 
-- Singleton for Environments
data SEnv a where
  Empty :: SEnv ()
  (:::) :: SType t -> SEnv e -> SEnv (e,t)
  AnySEnv :: SEnv a
    
instance SEq SEnv where
  Empty       =:= Empty       =  return Refl
  (t1 ::: e1) =:= (t2 ::: e2) =  do
    Refl <- t1 =:= t2
    Refl <- e1 =:= e2
    return Refl
  _           =:= _           =  fail ""  
    
data WExp where 
  WrapExp :: Exp e t -> SEnv e -> SType t -> WExp
 
convert :: UExp -> Maybe WExp
convert (UCon i)      =  return $ WrapExp (Con i) Empty SInt
convert (UAdd e1 e2)  =  do 
  WrapExp e1' env1 t1 <- convert e1
  WrapExp e2' env2 t2 <- convert e2  
  Refl <- env1 =:= env2
  Refl <- t1   =:= SInt 
  Refl <- t2   =:= SInt
  return $ WrapExp (Add e1' e2') env1 SInt
convert (UVar uv)     =  do
  WrapVar v env t <- convertVar uv  
  return $ WrapExp (Var v) env t
convert (UApp e1 e2)  =  do
  WrapExp e1' env1 tf <- convert e1
  WrapExp e2' env2 ta <- convert e2  
  Refl <- env1 =:= env2
  let tb = AnySType
  Refl <- tf   =:= (ta :-> tb)  
  return $ WrapExp (App e1' e2') env1 tb
convert (UAbs eb)     =  do
  WrapExp eb' envb tb <- convert eb
  let e  = AnySEnv
  let ta = AnySType    
  Refl <- envb =:= (ta ::: e)
  return $ WrapExp (Abs eb') e (ta :-> tb) 

data WVar where 
  WrapVar :: Var e t -> SEnv e -> SType t-> WVar 

convertVar :: UVar -> Maybe WVar 
convertVar  UZ      =  return $ WrapVar Z (AnySType ::: AnySEnv) AnySType
convertVar (US uv)  =  do 
  WrapVar v     e t <- convertVar uv
  return $ WrapVar (S v) (AnySType ::: e) t

data WVal where
  WrapVal :: a -> SType a -> WVal

weval :: WExp -> Maybe WVal
weval (WrapExp ex env t) = do 
  Refl <- env =:= Empty
  return $ WrapVal (eval ex ()) t 
                 
prop_correctConvert :: UExp -> Maybe Bool
prop_correctConvert ue = case ueval ue emptyUEnv of 
  I i -> do 
    WrapVal r t <- (weval <=< convert) ue
    Refl <- t =:= SInt
    return $ i == r
  F f -> do
    WrapVal r t <- (weval <=< convert) ue
    Refl <- t =:= (SInt :-> SInt)
    return $  and [f (I i) == I (r i) 
                  | i <- [-100..100]]
   
pcheck1 :: Bool
pcheck1 = prop_correctConvert udouble == Just True

pcheck2 :: Bool
pcheck2 = prop_correctConvert ufour == Just True