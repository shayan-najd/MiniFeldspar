module Expression.STLC.ADTValue where

import ErrorMonad

-- Values
data Val =
    Con Integer
  | Abs (Val -> Val)
    
con :: Integer -> ErrM Val
con = return . Con

abs :: (Val -> Val) -> ErrM Val
abs = return . Abs

app :: Val -> Val -> ErrM Val
app (Abs vf) va = return (vf va)
app _        _  = fail "Type Error!"

add :: Val -> Val -> ErrM Val
add (Con i) (Con j) = return (Con (i + j))
add _       _       = fail "Type Error!"
