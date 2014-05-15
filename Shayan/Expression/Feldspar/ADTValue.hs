module Expression.Feldspar.ADTValue where

import Prelude (error)
import MyPrelude 

data Exp = ConI Integer
         | ConB Bool  
         | ConF Float  
         | Abs (Exp -> Exp)
         | Tpl (Exp , Exp)  
         | Ary (Array Integer Exp)  
         | Cmx (Complex Float)  
 
class Lft t where
  lft :: t -> Exp
  
instance Lft Integer where  
  lft = ConI
  
instance Lft Bool where  
  lft = ConB

instance Lft Float where  
  lft = ConF

instance (CoLft a , Lft b) => Lft (a -> b) where
  lft f = Abs (lft . f . colft)

instance (Lft a , Lft b) => Lft (a , b) where
  lft (x , y) = Tpl (lft x , lft y) 
  
instance Lft a => Lft (Array Integer a) where  
  lft a = Ary (fmap lft a)
  
instance Lft (Complex Float) where  
  lft = Cmx

class CoLft t where
  colft :: Exp -> t
  
instance CoLft Integer where  
  colft (ConI i) = i
  colft _        = error "Type Error!"
  
instance CoLft Bool where  
  colft (ConB b) = b
  colft _        = error "Type Error!"
  
instance CoLft Float where  
  colft (ConF f) = f
  colft _        = error "Type Error!"

instance (Lft a , CoLft b) => CoLft (a -> b) where
  colft (Abs f) = colft . f . lft 
  colft _       = error "Type Error!"

instance (CoLft a , CoLft b) => CoLft (a , b) where
  colft (Tpl (x , y) ) = (colft x , colft y)
  colft _              = error "Type Error!"
  
instance CoLft a => CoLft (Array Integer a) where  
  colft (Ary x) = fmap colft x
  colft _       = error "Type Error!"
  
instance CoLft (Complex Float) where  
  colft (Cmx c) = c
  colft _       = error "Type Error!"

var :: a -> ErrM a
var = return

conI :: Integer -> ErrM Exp
conI = return . ConI

conB :: Bool -> ErrM Exp
conB = return . ConB

conF :: Float -> ErrM Exp
conF = return . ConF

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
ary (ConI l) vf = return (Ary (listArray (0 , (l - 1)) 
                               [vf (ConI i)
                               | i <- [0 .. (l - 1)]]))
ary _        _  = fail "Type Error!"             

len :: Exp -> ErrM Exp
len (Ary a) = (return . ConI . (1 +) . uncurry (flip (-)) . bounds) a
len _       = fail " Type Error!"

ind :: Exp -> Exp -> ErrM Exp
ind (Ary a) (ConI i) = return (a ! i)
ind _       _        = fail " Type Error!"

cmx :: Exp -> Exp -> ErrM Exp 
cmx (ConF fr) (ConF fi) = return (Cmx (fr :+ fi))
cmx _         _         = fail " Type Error!"