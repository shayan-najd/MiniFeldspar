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
abs = return . Abs . (frmRgt .)

-- Application of two values
app :: Val -> Val -> ErrM Val
app (Abs vf) va = return (vf va)
app _        _  = fail "Type Error!"

-- Addition of two values
add :: Val -> Val -> ErrM Val
add (Con i) (Con j) = return (Con (i + j))
add _       _       = fail "Type Error!"
