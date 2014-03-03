module TypeChecking.STLC.ADTExplicit where

import TypeChecking 
import InferenceMonad
import Expression.STLC.ADTExplicit
import Environment.ADT 
import Data.Nat
import Type.Herbrand hiding (App)

instance Chk Exp where
  type Cns Exp        = Zro ': Suc (Suc Zro) ': '[]
  chk e r = case e of
    Con t _     -> do addC (t :~: int)
                      return t
    Var t x     -> do t' <- get x r
                      addC (t :~: t')
                      return t
    Abs t eb    -> do taa <- newMT
                      tbb <- newMT
                      tb  <- chk eb (taa : r)
                      addC (t  :~: arr taa tbb)  
                      addC (tb :~: tbb)
                      return t
    App t ef ea -> do tf  <- chk ef r
                      ta  <- chk ea r
                      tfa <- newMT
                      tfb <- newMT
                      addC (tf :~: arr tfa tfb) 
                      addC (ta :~: tfa)
                      addC (t  :~: tfb)
                      return t
    Add t el er -> do tl <- chk el r
                      tr <- chk er r
                      addC (tl :~: int)
                      addC (tr :~: int)
                      addC (t  :~: int)
                      return t