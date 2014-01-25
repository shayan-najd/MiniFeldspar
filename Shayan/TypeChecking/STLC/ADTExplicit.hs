{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module TypeChecking.STLC.ADTExplicit where

import Expression.STLC.ADTExplicit
import Unification as U
import Environment.ADT as E
import TypeChecking 
import Data.Vector
import Data.Nat.GADT

instance (TypCons a ~ (Zro, (Suc (Suc Zro), r0)), Uni a) => 
         Chk (Exp a) where
  type Env (Exp a)   = E.Env a
  type Typ (Exp a)   = a
  chk (Con t _)     _ = do _ <- eqlCon intVar t 
                           return t
  chk (Var t x)     r = do t' <- get x r
                           eql t t'
                           return t
  chk (Abs t eb)    r = do ta ::: tb ::: Nil <- eqlCon arrVar t    
                           tb'               <- chk eb (ta : r)
                           eql tb tb'
                           return t
  chk (App t ef ea) r = do tf                  <- chk ef r
                           ta'                 <- chk ea r
                           (ta ::: tb ::: Nil) <- eqlCon arrVar tf
                           eql ta ta' 
                           eql t  tb
                           return t
  chk (Add t el er) r = do tl <- chk el r
                           tr <- chk er r
                           _ <- eqlCon intVar tl 
                           _ <- eqlCon intVar tr 
                           _ <- eqlCon intVar t 
                           return t