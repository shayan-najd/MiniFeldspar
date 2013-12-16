{-# OPTIONS_GHC -Wall #-}
module Value where

import ErrorMonad

-- Values
data Val =
    Num Int
  | Fun (Val -> Val)
    
-- Application of two values
app :: Val -> Val -> ErrM Val
app (Fun f) v  = return (f v)
app _       _  = fail "Type Error!"

-- Addition of two values
add :: Val -> Val -> ErrM Val
add (Num i) (Num j) = return (Num (i + j))
add _       _       = fail "Type Error!"
