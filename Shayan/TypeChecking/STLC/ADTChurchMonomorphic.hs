{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module TypeChecking.STLC.ADTChurchMonomorphic where

import Expression.STLC.ADTChurchMonomorphic
import Type.STLC.ADTSimple as T
import Environment.ADT as E
import TypeChecking

instance Chk Exp where
  type Env Exp = E.Env T.Typ
  type Typ Exp = T.Typ
  type Mnd Exp = Monad 
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