{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Unification where

import qualified Variable.GADT as V
import Data.Vector
import Data.Nat.GADT  
  
-- A type class providing operations required for type unification.      
class Monad (Mnd t) => Uni t where 
  type Mnd t :: * -> *
  type TypCons t      
  typCon :: V.Var (TypCons t) n -> Vec n t -> Mnd t t
  eql    :: t -> t -> Mnd t ()
  eqlCon :: V.Var (TypCons t) n -> t -> Mnd t (Vec n t)
  
int :: V.Var (Zro,r) Zro
int = V.Zro

arr :: V.Var (EnvIntArr r) (Suc (Suc Zro))
arr = V.Suc V.Zro  

bol :: V.Var (EnvIntArr (Zro , r)) Zro
bol = V.Suc (V.Suc V.Zro)

tpl :: V.Var (EnvIntArr (Zro , (Suc (Suc Zro) , r))) (Suc (Suc Zro))
tpl = V.Suc (V.Suc (V.Suc V.Zro))

ary :: V.Var (EnvFld r) (Suc Zro)
ary = V.Suc (V.Suc (V.Suc (V.Suc V.Zro)))

type EnvIntArr r = (Zro               -- Int has no  argument
                  ,(Suc (Suc Zro)     -- Arr has two arguments
                  ,r)) 
                   
type EnvFld    r = EnvIntArr 
                   (Zro
                  ,(Suc (Suc Zro) 
                  ,(Suc Zro 
                  ,r)))
