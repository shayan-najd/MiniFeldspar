{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds #-}
module TypeChecking.STLC.ADTChurch where
 
import Expression.STLC.ADTChurch  
import Unification as U
import Environment.ADT as E
import TypeChecking
import Data.Vector
import Data.Nat

instance (TypCons a ~ (Zro ': Suc (Suc Zro) ': r'), Uni a) => 
         Chk (Exp a) where
  type Env (Exp a)  = E.Env a
  type Typ (Exp a)  = a
  chk (Con _)     _ = typCon intVar Nil
  chk (Var x)     r = get x r
  chk (Abs ta eb) r = do tb <- chk eb (ta : r)
                         typCon arrVar (ta  ::: tb ::: Nil)
  chk (App ef ea) r = do tf                <- chk ef r
                         ta'               <- chk ea r
                         ta ::: tb ::: Nil <- eqlCon arrVar tf 
                         eql ta ta' 
                         return tb
  chk (Add el er) r = do tl <- chk el r
                         tr <- chk er r
                         _ <- eqlCon intVar tl 
                         _ <- eqlCon intVar tr 
                         typCon intVar Nil