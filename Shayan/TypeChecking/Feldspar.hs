module TypeChecking.Feldspar where
 
import Expression.Feldspar.GADTTyped
import Data.Vector
import TypeChecking
import Data.Nat  
import InferenceMonad
import Type.Herbrand hiding (App)

instance Chk (Exp n) where
  type Cns (Exp n)      = Zro ': Suc (Suc Zro) ': Zro ': Suc (Suc Zro) ': 
                          Suc Zro ': '[]
  type Env (Exp n)      = Vec n                          
  chk ee r = case ee of  
    ConI _         -> return int
    ConB _         -> return bol
    Var x          -> return (get x r)
    Abs eb         -> do ta <- newMT
                         tb <- chk eb (ta ::: r)
                         return (arr ta tb)                    
    App t  ef ea   -> do tf  <- chk ef r
                         ta  <- chk ea r
                         tfb <- newMT
                         addC (tf :~: arr t tfb)
                         addC (t  :~: ta)
                         return tfb                          
    Cnd ec et ef   -> do tc <- chk ec r                         
                         tt <- chk et r
                         tf <- chk ef r
                         addC (tc :~: bol)
                         addC (tt :~: tf)
                         return tt
    Whl ec eb ei   -> do t  <- newMT
                         tc <- chk ec (t ::: r)                         
                         tb <- chk eb (t ::: r)
                         ti <- chk ei r
                         addC (tc :~: bol)
                         addC (tb :~: t)
                         addC (ti :~: t)
                         return t                            
    Tpl ef es      -> do tf  <- chk ef r                            
                         ts  <- chk es r
                         return (tpl tf ts)
    Fst ts e       -> do t   <- chk e r                       
                         tf  <- newMT
                         addC (t  :~: tpl tf ts)
                         return tf                         
    Snd tf e       -> do t   <- chk e r                       
                         ts  <- newMT
                         addC (t  :~: tpl tf ts)
                         return ts                          
    Ary el ef      -> do tl  <- chk el r                            
                         tf  <- chk ef (int ::: r)
                         tfa <- newMT
                         addC (tl :~: int)
                         addC (tf :~: tfa)
                         return (ary tfa)
    Len ta e       -> do t   <- chk e r                         
                         addC (t  :~: ary ta)
                         return int
    Ind ea ei      -> do ta  <- chk ea r                       
                         ti  <- chk ei r
                         taa <- newMT
                         addC (ta :~: ary taa) 
                         addC (ti :~: int)
                         return taa                         
    Let t  el eb   -> do tl  <- chk el r 
                         tb  <- chk eb (t ::: r)                          
                         addC (t :~: tl)
                         return tb