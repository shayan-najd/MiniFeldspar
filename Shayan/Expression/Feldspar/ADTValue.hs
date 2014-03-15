module Expression.Feldspar.ADTValue where

import Prelude (error)
import MyPrelude 

data Val = ConI Integer
         | ConB Bool  
         | Abs (Val -> Val)
         | Tpl (Val , Val)  
         | Arr (Array Integer Val)  
    
var :: a -> ErrM a
var = return

conI :: Integer -> ErrM Val
conI = return . ConI

conB :: Bool -> ErrM Val
conB = return . ConB

abs :: (Val -> Val) -> ErrM Val
abs = return . Abs 

app :: Val -> Val -> ErrM Val
app (Abs vf) va = return (vf va)
app _        _  = fail "Type Error!"

addV :: Val
addV = Abs (\ (ConI vl) -> Abs (\ (ConI vr) -> ConI (vl + vr)))

add :: Val -> Val -> Val
add (ConI i) (ConI j) = ConI (i + j)
add _         _       = error "Type Error!"

cnd :: Val -> Val -> Val -> ErrM Val
cnd (ConB vc) v1 v2 = return (if vc then v1 else v2)
cnd _         _  _  = fail "Type Error!"                

whl :: (Val -> Val) -> (Val -> Val) -> Val -> ErrM Val
whl fc fb v = return (head (dropWhile 
                            (\ x -> case fc x of
                                ConB b -> b
                                _      -> error "Type Error!") 
                            (iterate fb v)))
              
fst :: Val -> ErrM Val
fst (Tpl (vf , _ )) = return vf
fst _               = fail "Type Error!"

snd :: Val -> ErrM Val
snd (Tpl (_  , vs)) = return vs
snd _               = fail "Type Error!"

tpl :: Val -> Val -> ErrM Val
tpl vf vs = return (Tpl (vf , vs))
 
ary :: Val -> (Val -> Val) -> ErrM Val
ary (ConI l) vf = return (Arr (listArray (0 , l) 
                               [vf (ConI i)
                               | i <- [0 .. l]]))
ary _        _  = fail "Type Error!"             

len :: Val -> ErrM Val
len (Arr a) = (return . ConI . (1 +) . uncurry (flip (-)) . bounds) a
len _       = fail " Type Error!"

ind :: Val -> Val -> ErrM Val
ind (Arr a) (ConI i) = return (a ! i)
ind _       _        = fail " Type Error!"

