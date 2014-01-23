{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module TypeChecking.STLC.ADTExplicitPolymorphic where

import Expression.STLC.ADTExplicitPolymorphic
import Unification as U
import Environment.ADT as E
import TypeChecking 
import Data.Vector
import Data.Nat.GADT

instance (TypCons a ~ (Zro, (Suc (Suc Zro), r0)), Uni a) => 
         Chk (Exp a) where
  type Env (Exp a)   = E.Env a
  type Typ (Exp a)   = a
  chk (Con t _)     _ = do _ <- eqlCon int t 
                           return t
  chk (Var t x)     r = do t' <- get x r
                           eql t t'
                           return t
  chk (Abs t eb)    r = do ta ::: tb ::: Nil <- eqlCon arr t    
                           tb'               <- chk eb (ta : r)
                           eql tb tb'
                           return t
  chk (App t ef ea) r = do tf                  <- chk ef r
                           ta'                 <- chk ea r
                           (ta ::: tb ::: Nil) <- eqlCon arr tf
                           eql ta ta' 
                           eql t  tb
                           return t
  chk (Add t el er) r = do tl <- chk el r
                           tr <- chk er r
                           _ <- eqlCon int tl 
                           _ <- eqlCon int tr 
                           _ <- eqlCon int t 
                           return t