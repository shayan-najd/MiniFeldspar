{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds, FlexibleContexts
           , PolyKinds, UndecidableInstances #-}
module TypeChecking.STLC.ADTChurch where
 
import Expression.STLC.ADTChurch  
import Environment.ADT as A
import TypeChecking
import Data.Nat
import InferenceMonad
import Type.Herbrand hiding (App)

instance Chk Exp where
  type Cns Exp      = 'Zro ': 'Suc ('Suc 'Zro) ': '[]
  chk (Con _)     _ = return int
  chk (Var x)     r = get x r
  chk (Abs ta eb) r = do tb  <- chk eb (ta : r)
                         return (arr ta tb)
  chk (App ef ea) r = do tf  <- chk ef r
                         ta  <- chk ea r
                         tfa <- newMT
                         tfb <- newMT
                         addC (arr tfa tfb :~: tf )
                         addC (ta :~: tfa) 
                         return tfb
  chk (Add el er) r = do tl <- chk el r
                         tr <- chk er r
                         addC (int :~: tl)
                         addC (int :~: tr)
                         return int 