{-# OPTIONS_GHC -Wall #-}
module ADTChurchMonomorphic where

import Expression.ADTChurchMonomorphic
import Type.ADTSimple
import Variable.ADT
import Environment.ADT
import Value.ADT
import ErrorMonad

-- Evaluation of expressions under specific environment of values 
evl :: Exp -> [Val] -> ErrM Val
evl (Con i)     _ = return (Num i)
evl (Var x)     r = get x r
evl (Abs _  eb) r = return (Fun (\ va -> case evl eb (va : r) of 
                                    Rgt vb -> vb
                                    Lft s  -> error s))
evl (App ef ea) r = do vf <- evl ef r
                       va <- evl ea r
                       vf `app` va 
evl (Add el er) r = do vl <- evl el r 
                       vr <- evl er r      
                       vl `add` vr

-- Typechecking and returning the type, if successful
chk :: Exp -> [Typ] -> ErrM Typ 
chk (Con _)     _ = return Int
chk (Var x)     r = get x r
chk (Abs ta eb) r = do tb <- chk eb (ta : r)
                       return (ta `Arr` tb)
chk (App ef ea) r = do ta `Arr` tb <- chk ef r
                       ta'         <- chk ea r
                       ta === ta' 
                       return tb
chk (Add el er) r = do tl <- chk el r
                       tr <- chk er r
                       tl === Int
                       tr === Int
                       return Int

-- An example expression doubling the input number
dbl :: Exp
dbl = Abs Int (Var Zro `Add` Var Zro)

-- An example expression composing two types
compose :: Typ -> Typ -> Typ -> Exp
compose ta tb tc = Abs (Arr tb tc) 
                (Abs (Arr ta tb) 
                 (Abs ta
                  (Var (Suc (Suc Zro)) `App` (Var (Suc Zro) `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp
four = (compose Int Int Int `App` dbl `App` dbl) `App` (Con 1)

-- Two simple test cases
test :: Bool
test = (case evl four [] of 
          Rgt (Num 4) -> True
          _           -> False) 
       && (chk four [] == Rgt Int)