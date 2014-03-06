module TypeChecking.STLC where
 
import Expression.STLC.ADTChurch  
import Environment.ADT 
import TypeChecking
import Data.Nat
import InferenceMonad
import Type.Herbrand hiding (App)

instance Chk Exp where
  type Cns Exp = Zro ': Suc (Suc Zro) ': '[]
  type Env Exp = []
  chk e r = case e of
    Con _     -> return int
    Var x     -> get x r
    Abs ta eb -> do tb  <- chk eb (ta : r)
                    return (arr ta tb)
    App ef ea -> do tf  <- chk ef r
                    ta  <- chk ea r
                    tfa <- newMT
                    tfb <- newMT
                    addC (arr tfa tfb :~: tf )
                    addC (ta :~: tfa) 
                    return tfb
    Add el er -> do tl <- chk el r
                    tr <- chk er r
                    addC (int :~: tl)
                    addC (int :~: tr)
                    return int 