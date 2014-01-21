{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module TypeChecking.Feldspar.ADTChurchPolymorphic where
 
import Expression.Feldspar.ADTChurchPolymorphic  
import Unification as U
import Environment.ADT as E
import TypeChecking
import Data.Vector 
import Data.Nat.GADT  

instance (TypCons a ~ (Zro, (Suc (Suc Zro), (Zro, (Suc (Suc Zro), 
                       (Suc Zro, r'))))) , Uni a) => 
         Chk (Exp a) where
  type Env (Exp a) = E.Env a
  type Typ (Exp a) = a
  chk (ConI _)    _ = typCon int Nil
  chk (ConB _)    _ = typCon bol Nil
  chk (Var x)     r = get x r
  chk (Abs ta eb) r = do tb <- chk eb (ta : r)
                         typCon arr (ta ::: tb ::: Nil)
                         
  chk (App ef ea) r = do tf        <- chk ef r
                         ta'       <- chk ea r
                         ta ::: tb ::: Nil <- eqlCon arr tf 
                         eql ta ta' 
                         return tb
                         
  chk (Cnd ec et ef) r = do tc <- chk ec r                         
                            tt <- chk et r
                            tf <- chk ef r
                            _  <- eqlCon bol tc
                            eql tt tf
                            return tt
                            
  chk (Whl ec eb ei) r = do tc          <- chk ec r                         
                            tb          <- chk eb r
                            ti          <- chk ei r
                            tca ::: tcb ::: Nil <- eqlCon arr tc
                            tba ::: tbb ::: Nil <- eqlCon arr tb
                            _                   <- eqlCon bol tcb
                            eql ti tca
                            eql ti tba
                            eql ti tbb
                            return ti                             
                            
  chk (Tpl ef es) r = do tf <- chk ef r                            
                         ts <- chk es r
                         typCon tpl (tf ::: ts ::: Nil)
                         
  chk (Fst e)     r = do t                <- chk e r                       
                         tf ::: _ ::: Nil <- eqlCon tpl t
                         return tf
                         
  chk (Snd e)     r = do t                <- chk e r                       
                         _ ::: ts ::: Nil <- eqlCon tpl t
                         return ts                          
                         
  chk (Ary el ef) r = do tl <- chk el r                            
                         tf <- chk ef r
                         tfa ::: tfb ::: Nil <- eqlCon arr tf
                         _                   <- eqlCon int tl
                         _                   <- eqlCon int tfa
                         typCon ary (tfb ::: Nil)
  
  chk (Len e)     r = do t <- chk e r                         
                         _ <- eqlCon ary t
                         typCon int Nil
                         
  chk (Ind ea ei) r = do ta            <- chk ea r                       
                         ti            <- chk ei r
                         (taa ::: Nil) <- eqlCon ary ta
                         _             <- eqlCon int ti
                         return taa
                         
  chk (Let el eb) r = do tl <- chk el r 
                         tb <- chk eb (tl : r)                          
                         return tb
                         
--  chk Any         _ = tCon "Any" [] 