{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module TypeChecking.Feldspar.ADTChurch where
 
import Expression.Feldspar.ADTChurch  
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
  chk (ConI _)    _ = typCon intVar Nil
  chk (ConB _)    _ = typCon bolVar Nil
  chk (Var x)     r = get x r
  chk (Abs ta eb) r = do tb <- chk eb (ta : r)
                         typCon arrVar (ta ::: tb ::: Nil)
                         
  chk (App ef ea) r = do tf        <- chk ef r
                         ta'       <- chk ea r
                         ta ::: tb ::: Nil <- eqlCon arrVar tf 
                         eql ta ta' 
                         return tb
                         
  chk (Cnd ec et ef) r = do tc <- chk ec r                         
                            tt <- chk et r
                            tf <- chk ef r
                            _  <- eqlCon bolVar tc
                            eql tt tf
                            return tt
                            
  chk (Whl ec eb ei) r = do tc          <- chk ec r                         
                            tb          <- chk eb r
                            ti          <- chk ei r
                            tca ::: tcb ::: Nil <- eqlCon arrVar tc
                            tba ::: tbb ::: Nil <- eqlCon arrVar tb
                            _                   <- eqlCon bolVar tcb
                            eql ti tca
                            eql ti tba
                            eql ti tbb
                            return ti                             
                            
  chk (Tpl ef es) r = do tf <- chk ef r                            
                         ts <- chk es r
                         typCon tplVar (tf ::: ts ::: Nil)
                         
  chk (Fst e)     r = do t                <- chk e r                       
                         tf ::: _ ::: Nil <- eqlCon tplVar t
                         return tf
                         
  chk (Snd e)     r = do t                <- chk e r                       
                         _ ::: ts ::: Nil <- eqlCon tplVar t
                         return ts                          
                         
  chk (Ary el ef) r = do tl <- chk el r                            
                         tf <- chk ef r
                         tfa ::: tfb ::: Nil <- eqlCon arrVar tf
                         _                   <- eqlCon intVar tl
                         _                   <- eqlCon intVar tfa
                         typCon aryVar (tfb ::: Nil)
  
  chk (Len e)     r = do t <- chk e r                         
                         _ <- eqlCon aryVar t
                         typCon intVar Nil
                         
  chk (Ind ea ei) r = do ta            <- chk ea r                       
                         ti            <- chk ei r
                         (taa ::: Nil) <- eqlCon aryVar ta
                         _             <- eqlCon intVar ti
                         return taa
                         
  chk (Let el eb) r = do tl <- chk el r 
                         tb <- chk eb (tl : r)                          
                         return tb                         