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
    
instance SEq SEnv where
  Empty       =:= Empty       =  return Refl
  (t1 ::: e1) =:= (t2 ::: e2) =  do
    Refl <- t1 =:= t2
    Refl <- e1 =:= e2
    return Refl
  _           =:= _           =  fail ""  
    
data WExp where 
  WrapExp :: Exp e t -> SEnv e -> SType t -> WExp
 
convert :: UExp -> [WSType] -> Maybe WExp
convert (UCon i)      _    =  return $ WrapExp (Con i) Empty SInt
convert (UAdd e1 e2)  env  =  do 
  WrapExp e1' env1 t1 <- convert e1 env
  WrapExp e2' env2 t2 <- convert e2 env 
  Refl <- env1 =:= env2
  Refl <- t1   =:= SInt 
  Refl <- t2   =:= SInt
  return $ WrapExp (Add e1' e2') env1 SInt
convert (UVar uv)     env  =  do
  WrapVar v ev t <- convertVar uv env 
  return $ WrapExp (Var v) ev t
convert (UApp e1 e2)  env  =  do
  WrapExp e1' env1 (ta :-> tb) <- convert e1 env
  WrapExp e2' env2 ta'         <- convert e2 env  
  Refl <- env1 =:= env2
  Refl <- ta =:= ta'
  return $ WrapExp (App e1' e2') env1 tb
convert (UAbs eb)     env  =  do
  --let ta = undefined
  let ta = SInt -- <-- WARNING: This is a hack working if the language
                --              were first-order with Int being the only
                --              base type.
  WrapExp eb' (ta' ::: e) tb <- convert eb (WrapSType ta :env)
  Refl <- ta =:= ta'
  return $ WrapExp (Abs eb') e (ta :-> tb) 

data WVar where 
  WrapVar :: Var e t -> SEnv e -> SType t-> WVar 

data WSType where 
  WrapSType :: SType a -> WSType
 
convertVar :: UVar -> [WSType]-> Maybe WVar 
convertVar  UZ     [wt]      = do 
  case wt of
    WrapSType st -> return $ WrapVar Z (st ::: Empty) st
convertVar (US uv) (wt:env)  =  do 
  WrapVar v     e t <- convertVar uv env
  case wt of
    WrapSType st -> return $ WrapVar (S v) (st ::: e) t
convertVar _       _         = fail ""    

data WVal where
  WrapVal :: a -> SType a -> WVal

weval :: WExp -> Maybe WVal
weval (WrapExp ex env t) = do 
  Refl <- env =:= Empty
  return $ WrapVal (eval ex ()) t 
                 
prop_correctConvert :: UExp -> Maybe Bool
prop_correctConvert ue = case ueval ue emptyUEnv of 
  I i -> do 
    WrapVal r t <- (weval <=< flip convert []) ue
    Refl <- t =:= SInt
    return $ i == r
  F f -> do
    WrapVal r t <- (weval <=< flip convert []) ue
    Refl <- t =:= (SInt :-> SInt)
    return $  and [f (I i) == I (r i) 
                  | i <- [-100..100]]
   
pcheck1 :: Bool
pcheck1 = prop_correctConvert udouble == Just True

pcheck2 :: Bool
pcheck2 = prop_correctConvert ufour == Just True

