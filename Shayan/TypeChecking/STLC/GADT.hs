{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module TypeChecking.STLC.GADT where

import Expression.STLC.GADT
import Type.STLC.GADT   as G
import Environment.GADT as G
import TypeChecking 
 
instance Chk (Exp r t) where
  type Env (Exp r t) = G.Env G.Typ r
  type Typ (Exp r t) = G.Typ t
  type Mnd (Exp r t) = Monad
  chk (Con _)     _ = return Int
  chk (Var x)     r = return (gets x r)
  chk (Abs ta eb) r = do tb <- chk eb (ta `Ext` r)
                         return (ta `Arr` tb)
  chk (App ef _ ) r = do tf <- chk ef r
                         case tf  of 
                           Arr _ tb -> return tb
  chk (Add _  _ ) _ = return Int 