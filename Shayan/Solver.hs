module Solver where

import Singleton
import Type.Herbrand
import InferenceMonad
import ErrorMonad
import Prelude hiding (foldr,notElem)
import Data.Foldable
import Data.Vector

slv :: [Typ r] -> [HerCon r] -> ErrM [Typ r]
slv mem []                                   
  = return mem
-- Optional Optimization (assuming syntactic equlity of types):    
-- slv mem ((t :~: t') : cs) | t == t'  
--  = slv mem cs
slv mem ((App c ts :~: App c' ts') : cs)
  = do Rfl <- eqlSin (len ts) (len ts')
       if c == c' 
         then slv mem  (zipWith (:~:) (toList ts) (toList ts') ++ cs)  
         else fail "Type Error!"     
slv mem ((Mta i :~: t) : cs) | i `notElem` mtas t
  = slv (appTs i t mem) (appCs i t cs)
slv mem ((t :~: Mta i) : cs) | i `notElem` mtas t   
  = slv (appTs i t mem) (appCs i t cs)
slv _   ((_ :~: _ ) : _) 
  = fail "Type Error!" 
