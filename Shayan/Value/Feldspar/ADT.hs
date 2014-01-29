{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Value.Feldspar.ADT where

import ErrorMonad
import Data.Array

-- Values
data Val =
    ConI Integer
  | ConB Bool  
  | Abs (Val -> Val)
  | Tpl (Val , Val)  
  | Arr (Array Integer Val)  
    
conI :: Integer -> ErrM Val
conI = return . ConI

conB :: Bool -> ErrM Val
conB = return . ConB

abs :: (Val -> ErrM Val) -> ErrM Val
abs = return . Abs . (frmRgt .)

-- Application of two values
app :: Val -> Val -> ErrM Val
app (Abs vf) va = return (vf va)
app _        _  = fail "Type Error!"

addV :: Val
addV = Abs (\ (ConI vl) -> Abs (\ (ConI vr) -> ConI (vl + vr)))

-- Addition of two values
add :: Val -> Val -> Val
add (ConI i) (ConI j) = ConI (i + j)
add _         _       = error "Type Error!"

cnd :: Val -> Val -> Val -> ErrM Val
cnd (ConB vc) v1 v2 = return (if vc then v1 else v2)
cnd _         _  _  = fail "Type Error!"                

fst :: Val -> ErrM Val
fst (Tpl (vf , _ )) = return vf
fst _               = fail "Type Error!"

snd :: Val -> ErrM Val
snd (Tpl (_  , vs)) = return vs
snd _               = fail "Type Error!"

tpl :: Val -> Val -> ErrM Val
tpl vf vs = return (Tpl (vf , vs))
 
arr :: Val -> Val -> ErrM Val
arr (ConI l) vf = do vs <- sequence [app vf (ConI i)
                                    | i <- [0 .. l]] 
                     return (Arr (listArray (0 , l) vs))
arr _          _  = fail "Type Error!"             

len :: Val -> ErrM Val
len (Arr a) = (return . ConI . (1 +) . uncurry (flip (-)) . bounds) a
len _       = fail " Type Error!"

ind :: Val -> Val -> ErrM Val
ind (Arr a) (ConI i) = return (a ! i)
ind _       _        = fail " Type Error!"

whl :: Val -> Val -> Val -> ErrM Val
whl (Abs fc) (Abs fb) v = return (head (dropWhile 
                                        (\ x -> case fc x of
                                            ConB b -> b
                                            _      -> error "Type Error!") 
                                        (iterate fb v)))
whl _        _        _ = fail "Type Error!"                          