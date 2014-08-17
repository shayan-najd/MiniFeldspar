module InferenceMonad where

import MyPrelude

import Type.Herbrand
import Nat.ADT

type InfM r a = State (Nat , [HerCon r]) a

newMT :: InfM r (Typ r)
newMT = do (i , x) <- getState
           put (Suc i , x)
           return (Mta i)         
          
addC  :: HerCon r -> InfM r ()
addC c = modify (\ (i , cs) -> (i , c : cs))

