module TypeChecking.Feldspar where
 
import Prelude ()
import MyPrelude

import Expression.Feldspar.GADTTyped
import qualified Type.Herbrand as TH

import Environment.Scoped  as ES

import Nat.ADT  

import TypeChecking
import InferenceMonad

instance Chk (Exp n) where
  type Cns (Exp n)      = Zro ': Suc (Suc Zro) ': Zro ': Suc (Suc Zro) ': 
                          Suc Zro ': '[]
  type Env (Exp n)      = ES.Env n                          
  chk ee r = case ee of  
    ConI _         -> return TH.int
    ConB _         -> return TH.bol
    Var x          -> return (get x r)
    Abs eb         -> do ta <- newMT
                         tb <- chk eb (Ext ta r)
                         return (TH.arr ta tb)                    
    App t  ef ea   -> do tf  <- chk ef r
                         ta  <- chk ea r
                         tfb <- newMT
                         addC (tf TH.:~: TH.arr t tfb)
                         addC (t  TH.:~: ta)
                         return tfb                          
    Cnd ec et ef   -> do tc <- chk ec r                         
                         tt <- chk et r
                         tf <- chk ef r
                         addC (tc TH.:~: TH.bol)
                         addC (tt TH.:~: tf)
                         return tt
    Whl ec eb ei   -> do t  <- newMT
                         tc <- chk ec (Ext t r)                         
                         tb <- chk eb (Ext t r)
                         ti <- chk ei r
                         addC (tc TH.:~: TH.bol)
                         addC (tb TH.:~: t)
                         addC (ti TH.:~: t)
                         return t                            
    Tpl ef es      -> do tf  <- chk ef r                            
                         ts  <- chk es r
                         return (TH.tpl tf ts)
    Fst ts e       -> do t   <- chk e r                       
                         tf  <- newMT
                         addC (t  TH.:~: TH.tpl tf ts)
                         return tf                         
    Snd tf e       -> do t   <- chk e r                       
                         ts  <- newMT
                         addC (t  TH.:~: TH.tpl tf ts)
                         return ts                          
    Ary el ef      -> do tl  <- chk el r                            
                         tf  <- chk ef (Ext TH.int r)
                         tfa <- newMT
                         addC (tl TH.:~: TH.int)
                         addC (tf TH.:~: tfa)
                         return (TH.ary tfa)
    Len ta e       -> do t   <- chk e r                         
                         addC (t  TH.:~: TH.ary ta)
                         return TH.int
    Ind ea ei      -> do ta  <- chk ea r                       
                         ti  <- chk ei r
                         taa <- newMT
                         addC (ta TH.:~: TH.ary taa) 
                         addC (ti TH.:~: TH.int)
                         return taa                         
    Let t  el eb   -> do tl  <- chk el r 
                         tb  <- chk eb (Ext t r)                          
                         addC (t TH.:~: tl)
                         return tb