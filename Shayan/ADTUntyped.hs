{-# OPTIONS_GHC -Wall #-}
module ADTUntyped where
 
import Expression.ADTUntyped 
import Variable.ADT
import Environment.ADT
import Value.ADT
import ErrorMonad

-- Evaluation of expressions under specific environment of values 
evl :: Exp -> [Val] -> ErrM Val
evl (Con i)     _ = return (Num i)
evl (Var x)     r = get x r
evl (Abs eb)    r = return (Fun (\ va -> case evl eb (va : r) of 
                                    Rgt vb -> vb
                                    Lft s  -> error s))
evl (App ef ea) r = do vf <- evl ef r
                       va <- evl ea r
                       vf `app` va 
evl (Add el er) r = do vl <- evl el r 
                       vr <- evl er r      
                       vl `add` vr
 
-- An example expression doubling the input number
dbl :: Exp
dbl = Abs (Var Zro `Add` Var Zro)

-- An example expression composing two types
compose :: Exp
compose = Abs (Abs (Abs  (Var (Suc (Suc Zro)) 
                          `App` 
                          (Var (Suc Zro) 
                           `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp
four = (compose `App` dbl `App` dbl) `App` (Con 1)

-- Two simple test cases
test :: Bool
test = (case evl four [] of 
          Rgt (Num 4) -> True
          _           -> False) 

