{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module TypeCheckingClass where

import qualified Type.ADTSimple as AS
import qualified Type.ADTWithMetavariable as AM
import Solver
import ErrorMonad

import Control.Monad.State(State)

-- A type class providing operations required for type unification. The first 
-- argument (m) provides the monad required for unification of types (t).       
class Monad m => Chk m a where 
  tInt   :: m a
  tArr   :: a -> a -> m a
  eql    :: a -> a -> m ()
  eqlInt :: a -> m ()
  eqlArr :: a -> m (a , a)
  
instance Chk ErrM AS.Typ where
  tInt                  = return AS.Int
  tArr                  = (return .) . AS.Arr
  eql                   = (AS.===) 
  eqlInt                = eql AS.Int
  eqlArr (x `AS.Arr` y) = return (x , y)
  eqlArr _              = fail "Type Error!"  

-- Setting the checker to collect constraints wherever types are unified  
instance Chk (State (Int,[EqlC])) AM.Typ where
  tInt      = return AM.Int
  tArr      = (return .) . AM.Arr
  eql t  t' = addC (t :~: t') 
  eqlInt t  = eql t AM.Int 
  eqlArr t  = do t1 <- newMT
                 t2 <- newMT                 
                 addC (t :~: t1 `AM.Arr` t2)
                 return (t1 , t2)
  