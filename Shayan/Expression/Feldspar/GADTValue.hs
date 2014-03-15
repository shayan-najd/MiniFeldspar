module Expression.Feldspar.GADTValue where

import Prelude ()
import MyPrelude as P hiding (Int) 

import Singleton

import Type.Feldspar.ADT
import Type.Feldspar.GADT ()

data Val :: Typ -> * where 
  Val :: Trm t -> Val t
  
(===) ::(Eq t', Trm t ~ t') =>
        Val t -> Val t -> Bool 
(Val x) === (Val y) = x == y

getVal :: Val t -> Trm t
getVal (Val x) = x

var :: t -> t
var = id

conI :: Integer -> Val Int
conI = Val 
     
conB :: Bool -> Val Bol
conB = Val

abs :: (Trm ta -> Trm tb) -> Val (Arr ta tb)
abs = Val 
        
app :: Val (Arr ta tb) -> Val ta -> Val tb
app (Val vf) (Val va) = Val (vf va)

addV :: Val (Arr Int (Arr Int Int))
addV = Val (+)

cnd :: Val Bol -> Val a -> Val a -> Val a
cnd (Val vc) vt vf = if vc then vt else vf

whl :: Val (Arr s  Bol) -> Val (Arr s s) -> Val s -> Val s
whl (Val fc) (Val fb) =  Val . head . dropWhile fc . iterate fb . getVal 

tpl :: Val tf -> Val ts -> Val (Tpl tf ts)
tpl (Val vf) (Val vs) = Val (vf , vs)

fst :: Val (Tpl a b) -> Val a
fst (Val v) = Val (P.fst v)

snd :: Val (Tpl a b) -> Val b
snd (Val v) = Val (P.snd v)
 
ary :: Val Int -> Val (Arr Int a) -> Val (Ary a)
ary (Val vl) (Val vf) =  Val (listArray (0 , vl)  
                              [vf i | i <- [0 .. vl]])

len :: Val (Ary a) -> Val Int
len (Val e)  = (Val . (1 +) . uncurry (flip (-)) . bounds) e

ind :: Val (Ary a) -> Val Int -> Val a
ind (Val v) (Val vi) = Val (v ! vi)
 
lett :: Val tl -> Val (Arr tl tb) -> Val tb
lett (Val vl) (Val vb) = Val (vb vl)
  
