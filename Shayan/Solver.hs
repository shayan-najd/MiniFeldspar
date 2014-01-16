{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module Solver where

import SingletonEquality
import Type.Herbrand
import InferenceMonad
import ErrorMonad
import Prelude hiding (foldr,notElem)
import Data.Foldable
import SingletonEquality.DataNatGADT ()

-- Constraint Solving (Herbrand style constraint solving)
slv :: [Typ r] -> [EqlC r] -> ErrM [Typ r]
slv mem []                                   
  = return mem
-- Optional Optimization (assuming syntactic equlity of types):    
-- slv mem ((t :~: t') : cs) | t == t'  
--  = slv mem cs
slv mem ((App n c ts :~: App n' c' ts') : cs)
  = do Rfl <- eqlSin n n'
       if c == c' 
         then slv mem  (zipWith (:~:) (toList ts) (toList ts') ++ cs)  
         else fail "Type Error!"     
slv mem ((Mta i :~: t) : cs) | i `notElem` mtas t
  = slv (appTs i t mem) (appCs i t cs)
slv mem ((t :~: Mta i) : cs) | i `notElem` mtas t   
  = slv (appTs i t mem) (appCs i t cs)
slv _   ((_ :~: _ ) : _) 
  = fail "Type Error!" 
