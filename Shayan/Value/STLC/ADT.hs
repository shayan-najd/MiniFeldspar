{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
module Value.STLC.ADT where

import ErrorMonad

-- Values
data Val =
    Con Integer
  | Abs (Val -> Val)
    
con :: Integer -> ErrM Val
con = return . Con

abs :: (Val -> ErrM Val) -> ErrM Val
abs f = return (Abs (\ va -> case f va of 
                         Rgt vb -> vb
                         Lft s  -> error s))

-- Application of two values
app :: Val -> Val -> ErrM Val
app (Abs f) v  = return (f v)
app _       _  = fail "Type Error!"

-- Addition of two values
add :: Val -> Val -> ErrM Val
add (Con i) (Con j) = return (Con (i + j))
add _       _       = fail "Type Error!"
