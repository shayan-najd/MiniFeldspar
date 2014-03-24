module Expression.Feldspar.ADTValue where

import Prelude (error)
import MyPrelude 

data Exp = ConI Integer
         | ConB Bool  
         | Abs (Exp -> Exp)
         | Tpl (Exp , Exp)  
         | Ary (Array Integer Exp)  
    
var :: a -> ErrM a
var = return

conI :: Integer -> ErrM Exp
conI = return . ConI

conB :: Bool -> ErrM Exp
conB = return . ConB

abs :: (Exp -> Exp) -> ErrM Exp
abs = return . Abs 

app :: Exp -> Exp -> ErrM Exp
app (Abs vf) va = return (vf va)
app _        _  = fail "Type Error!"

addV :: Exp
addV = Abs (\ (ConI vl) -> Abs (\ (ConI vr) -> ConI (vl + vr)))

add :: Exp -> Exp -> Exp
add (ConI i) (ConI j) = ConI (i + j)
add _         _       = error "Type Error!"

cnd :: Exp -> Exp -> Exp -> ErrM Exp
cnd (ConB vc) v1 v2 = return (if vc then v1 else v2)
cnd _         _  _  = fail "Type Error!"                

whl :: (Exp -> Exp) -> (Exp -> Exp) -> Exp -> ErrM Exp
whl fc fb v = return (head (dropWhile 
                            (\ x -> case fc x of
                                ConB b -> b
                                _      -> error "Type Error!") 
                            (iterate fb v)))
              
fst :: Exp -> ErrM Exp
fst (Tpl (vf , _ )) = return vf
fst _               = fail "Type Error!"

snd :: Exp -> ErrM Exp
snd (Tpl (_  , vs)) = return vs
snd _               = fail "Type Error!"

tpl :: Exp -> Exp -> ErrM Exp
tpl vf vs = return (Tpl (vf , vs))
 
ary :: Exp -> (Exp -> Exp) -> ErrM Exp
ary (ConI l) vf = return (Ary (listArray (0 , l) 
                               [vf (ConI i)
                               | i <- [0 .. l]]))
ary _        _  = fail "Type Error!"             

len :: Exp -> ErrM Exp
len (Ary a) = (return . ConI . (1 +) . uncurry (flip (-)) . bounds) a
len _       = fail " Type Error!"

ind :: Exp -> Exp -> ErrM Exp
ind (Ary a) (ConI i) = return (a ! i)
ind _       _        = fail " Type Error!"

