{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module TypeChecking.Feldspar.GADT where
 
import Expression.Feldspar.GADT  
import qualified Type.Feldspar.GADT   as G
import Environment.GADT as E
import TypeChecking
 
instance Chk (Exp r t) where
  type Env (Exp r t) = E.Env G.Typ r
  type Typ (Exp r t) = G.Typ t
  type Mnd (Exp r t) = Monad
  chk (ConI _)    _ = return G.Int 
  chk (ConB _)    _ = return G.Bol
  chk (Var x)     r = return (gets x r)
  chk (Abs ta eb) r = do tb <- chk eb (ta `Ext` r)
                         return (ta `G.Arr` tb)
                         
  chk (App ef _ )  r = do G.Arr _ tb <- chk ef r
                          return tb
                         
  chk (Cnd _ et _) r = do tt <- chk et r                         
                          return tt
                           
  chk (Whl ec _ _) r = do G.Arr ts _  <- chk ec r                         
                          return ts
                             
  chk (Tpl ef es)  r = do tf <- chk ef r                            
                          ts <- chk es r
                          return (G.Tpl tf ts)

  chk (Fst e)      r = do G.Tpl tf _  <- chk e r                       
                          return tf
        
  chk (Snd e)      r = do G.Tpl _  ts <- chk e r                       
                          return ts                          
                     
  chk (Ary _   ef) r = do G.Arr _  ta <- chk ef r
                          return (G.Ary ta)
                          
  chk (Len _)      _ = return G.Int
                         
  chk (Ind ea _)   r = do G.Ary ta <- chk ea r                         
                          return ta
                         
  chk (Let el eb)  r = do tl <- chk el r 
                          tb <- chk eb (tl `Ext` r)                          
                          return tb
                         
 