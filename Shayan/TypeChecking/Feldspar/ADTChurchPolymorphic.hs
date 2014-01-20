{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module TypeChecking.Feldspar.ADTChurchPolymorphic where
 
import Expression.Feldspar.ADTChurchPolymorphic  
import Unification as U
import Environment.ADT as E
import TypeChecking
 
instance Uni a => Chk (Exp a) where
  type Env (Exp a) = E.Env a
  type Typ (Exp a) = a
  chk (ConI _)    _ = tCon "Int" []
  chk (ConB _)    _ = tCon "Bol" []
  chk (Var x)     r = get x r
  chk (Abs ta eb) r = do tb <- chk eb (ta : r)
                         tCon "Arr" [ta , tb]
                         
  chk (App ef ea) r = do tf        <- chk ef r
                         ta'       <- chk ea r
                         [ta , tb] <- eqlCon "Arr" tf 
                         ta `eql` ta' 
                         return tb
                         
  chk (Cnd ec et ef) r = do tc <- chk ec r                         
                            tt <- chk et r
                            tf <- chk ef r
                            [] <- eqlCon "Bol" tc
                            tt `eql` tf
                            return tt
                            
  chk (Whl ec eb ei) r = do tc          <- chk ec r                         
                            tb          <- chk eb r
                            ti          <- chk ei r
                            [tca , tcb] <- eqlCon "Arr" tc
                            [tba , tbb] <- eqlCon "Arr" tb
                            []          <- eqlCon "Bol" tcb
                            ti  `eql` tca
                            ti  `eql` tba
                            ti  `eql` tbb
                            return ti                             
                            
  chk (Tpl ef es) r = do tf <- chk ef r                            
                         ts <- chk es r
                         tCon "Tpl" [tf , ts]
                         
  chk (Fst e)     r = do t        <- chk e r                       
                         [tf , _] <- eqlCon "Tpl" t
                         return tf
                         
  chk (Snd e)     r = do t        <- chk e r                       
                         [_ , ts] <- eqlCon "Tpl" t
                         return ts                          
                         
  chk (Ary el ef) r = do tl <- chk el r                            
                         tf <- chk ef r
                         [tfa , tfb] <- eqlCon "Arr" tf
                         []          <- eqlCon "Int" tl
                         []          <- eqlCon "Int" tfa
                         tCon "Ary" [tfb]
  
  chk (Len e)     r = do t <- chk e r                         
                         [_] <- eqlCon "Ary" t
                         tCon "Int" []
                         
  chk (Ind ea ei) r = do ta    <- chk ea r                       
                         ti    <- chk ei r
                         [taa] <- eqlCon "Ary" ta
                         []    <- eqlCon "Int" ti
                         return taa
                         
  chk (Let el eb) r = do tl <- chk el r 
                         tb <- chk eb (tl : r)                          
                         return tb
                         
--  chk Any         _ = tCon "Any" [] 