{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies, TypeOperators , DataKinds #-}
module TypeChecking.Feldspar.ADTChurch where
 
import Expression.Feldspar.ADTChurch  
import Environment.ADT 
import TypeChecking
import Data.Nat  
import InferenceMonad
import Type.Herbrand hiding (App)

instance Chk Exp where
  type Cns Exp      = Zro ': Suc (Suc Zro) ': Zro ': Suc (Suc Zro) ': 
                      Suc Zro ': '[]
  chk (ConI _)    _ = return int
  chk (ConB _)    _ = return bol
  chk (Var x)     r = get x r
  chk (Abs ta eb) r = do tb <- chk eb (ta : r)
                         return (arr ta tb)                    
  chk (App ef ea) r = do tf  <- chk ef r
                         ta  <- chk ea r
                         tfa <- newMT
                         tfb <- newMT
                         addC (tf :~: arr tfa tfb)
                         addC (ta :~: tfa)
                         return tfb                          
  chk (Cnd ec et ef) r = do tc <- chk ec r                         
                            tt <- chk et r
                            tf <- chk ef r
                            addC (tc :~: bol)
                            addC (tt :~: tf)
                            return tt
  chk (Whl ec eb ei) r = do tc <- chk ec r                         
                            tb <- chk eb r
                            ti <- chk ei r
                            tca <- newMT
                            tcb <- newMT
                            addC (tc  :~: arr tca tcb)
                            tba <- newMT
                            tbb <- newMT
                            addC (tb  :~: arr tba tbb)
                            addC (tcb :~: bol)
                            addC (ti  :~: tca)
                            addC (ti  :~: tba)
                            addC (ti  :~: tbb)
                            return ti                            
  chk (Tpl ef es) r = do tf  <- chk ef r                            
                         ts  <- chk es r
                         return (tpl tf ts)
  chk (Fst e)     r = do t   <- chk e r                       
                         tf  <- newMT
                         ts  <- newMT
                         addC (t  :~: tpl tf ts)
                         return tf                         
  chk (Snd e)     r = do t   <- chk e r                       
                         tf  <- newMT
                         ts  <- newMT
                         addC (t  :~: tpl tf ts)
                         return ts                          
  chk (Ary el ef) r = do tl  <- chk el r                            
                         tf  <- chk ef r
                         tfa <- newMT
                         tfb <- newMT
                         addC (tf  :~: arr tfa tfb)
                         addC (tl  :~: int)
                         addC (tfa :~: int)
                         return (ary tfb)
  chk (Len e)     r = do t   <- chk e r                         
                         ta  <- newMT
                         addC (t   :~: ary ta)
                         return int
  chk (Ind ea ei) r = do ta  <- chk ea r                       
                         ti  <- chk ei r
                         taa <- newMT
                         addC (ta :~: ary taa) 
                         addC (ti :~: int)
                         return taa                         
  chk (Let el eb) r = do tl  <- chk el r 
                         tb  <- chk eb (tl : r)                          
                         return tb                         