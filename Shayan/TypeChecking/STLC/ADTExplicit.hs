{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators #-}
module TypeChecking.STLC.ADTExplicit where

import Expression.STLC.ADTExplicit
import Environment.ADT as E
import TypeChecking 
import Data.Nat
import Type.Herbrand hiding (App)
import InferenceMonad

instance Chk Exp where
  type Cns Exp        = Zro ': Suc (Suc Zro) ': '[]
  chk (Con t _)     _ = do addC (t :~: int)
                           return t
  chk (Var t x)     r = do t' <- get x r
                           addC (t :~: t')
                           return t
  chk (Abs t eb)    r = do taa <- newMT
                           tbb <- newMT
                           tb  <- chk eb (taa : r)
                           addC (t  :~: arr taa tbb)  
                           addC (tb :~: tbb)
                           return t
  chk (App t ef ea) r = do tf  <- chk ef r
                           ta  <- chk ea r
                           tfa <- newMT
                           tfb <- newMT
                           addC (tf :~: arr tfa tfb) 
                           addC (ta :~: tfa)
                           addC (t  :~: tfb)
                           return t
  chk (Add t el er) r = do tl <- chk el r
                           tr <- chk er r
                           addC (tl :~: int)
                           addC (tr :~: int)
                           addC (t  :~: int)
                           return t