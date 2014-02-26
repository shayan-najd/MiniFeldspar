{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
module InferenceMonad where

import Control.Monad.State (State,get,put,modify)
import Conversion
import Type.Herbrand
import ErrorMonad
import Data.Nat
 
-- Subtitution of a metavariable ([i := t]) in a type equality constraint
appC :: Nat -> Typ r -> HerCon r -> HerCon r
appC i t (t1 :~: t2) = appT i t t1 :~: appT i t t2

-- Subtitution of a metavariable ([i := t]) in a list of constraints
appCs :: Nat -> Typ r -> [HerCon r] -> [HerCon r]
appCs i t = map (appC i t)

-- A monad for type inference carrying, as state, an integer for generating 
-- fresh names and a list of type equality constraints collected 
type InfM r a = State (Nat , [HerCon r]) a

newMta :: Cnv (Typ r) t => InfM r t
newMta = do t <- newMT
            let Rgt t' = cnv t
            return t'    

-- Generating fresh metavariables using fresh names
newMT :: InfM r (Typ r)
newMT = do (i , x) <- get
           put (succ i , x)
           return (Mta i)         
          
-- Adding a type equality constraint to the state
addC  :: HerCon r -> InfM r ()
addC c = modify (\ (i , cs) -> (i , c : cs))
