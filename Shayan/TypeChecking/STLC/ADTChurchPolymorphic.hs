{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module TypeChecking.STLC.ADTChurchPolymorphic where
 
import Expression.STLC.ADTChurchPolymorphic  
import Unification as U
import Environment.ADT as E
import TypeChecking

instance Uni a => Chk (Exp a) where
  type Env (Exp a)  = E.Env a
  type Typ (Exp a)  = a
  chk (Con _)     _ = tCon "Int" []
  chk (Var x)     r = get x r
  chk (Abs ta eb) r = do tb <- chk eb (ta : r)
                         tCon "Arr" [ta , tb]
  chk (App ef ea) r = do tf        <- chk ef r
                         ta'       <- chk ea r
                         [ta , tb] <- eqlCon "Arr" tf 
                         eql ta ta' 
                         return tb
  chk (Add el er) r = do tl <- chk el r
                         tr <- chk er r
                         [] <- eqlCon "Int" tl 
                         [] <- eqlCon "Int" tr 
                         tCon "Int" []