{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module TypeChecking.STLC.ADTChurchPolymorphic where
 
import Expression.STLC.ADTChurchPolymorphic  
import Unification as U
import Environment.ADT as E
import TypeChecking
import Data.Vector
import Data.Nat.GADT

instance (TypCons a ~ (Zro, (Suc (Suc Zro), r')), Uni a) => 
         Chk (Exp a) where
  type Env (Exp a)  = E.Env a
  type Typ (Exp a)  = a
  chk (Con _)     _ = typCon int Nil
  chk (Var x)     r = get x r
  chk (Abs ta eb) r = do tb <- chk eb (ta : r)
                         typCon arr (ta  ::: tb ::: Nil)
  chk (App ef ea) r = do tf                <- chk ef r
                         ta'               <- chk ea r
                         ta ::: tb ::: Nil <- eqlCon arr tf 
                         eql ta ta' 
                         return tb
  chk (Add el er) r = do tl <- chk el r
                         tr <- chk er r
                         _ <- eqlCon int tl 
                         _ <- eqlCon int tr 
                         typCon int Nil