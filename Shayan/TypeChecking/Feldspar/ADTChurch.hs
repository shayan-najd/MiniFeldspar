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
  chk ee r = case ee of  
    ConI _       -> return int
    ConB _       -> return bol
    Var x        -> get x r
    Abs ta eb    -> do tb <- chk eb (ta : r)
                       return (arr ta tb)                    
    App ef ea    -> do tf  <- chk ef r
                       ta  <- chk ea r
                       tfa <- newMT
                       tfb <- newMT
                       addC (tf :~: arr tfa tfb)
                       addC (ta :~: tfa)
                       return tfb                          
    Cnd ec et ef -> do tc <- chk ec r                         
                       tt <- chk et r
                       tf <- chk ef r
                       addC (tc :~: bol)
                       addC (tt :~: tf)
                       return tt
    Whl ec eb ei -> do tc <- chk ec r                         
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
    Tpl ef es    -> do tf  <- chk ef r                            
                       ts  <- chk es r
                       return (tpl tf ts)
    Fst e        -> do t   <- chk e r                       
                       tf  <- newMT
                       ts  <- newMT
                       addC (t  :~: tpl tf ts)
                       return tf                         
    Snd e        -> do t   <- chk e r                       
                       tf  <- newMT
                       ts  <- newMT
                       addC (t  :~: tpl tf ts)
                       return ts                          
    Ary el ef    -> do tl  <- chk el r                            
                       tf  <- chk ef r
                       tfa <- newMT
                       tfb <- newMT
                       addC (tf  :~: arr tfa tfb)
                       addC (tl  :~: int)
                       addC (tfa :~: int)
                       return (ary tfb)
    Len e        -> do t   <- chk e r                         
                       ta  <- newMT
                       addC (t   :~: ary ta)
                       return int
    Ind ea ei    -> do ta  <- chk ea r                       
                       ti  <- chk ei r
                       taa <- newMT
                       addC (ta :~: ary taa) 
                       addC (ti :~: int)
                       return taa                         
    Let el eb    -> do tl  <- chk el r 
                       tb  <- chk eb (tl : r)                          
                       return tb