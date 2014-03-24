module Expression.Feldspar.GADTValue where

import Prelude ()
import MyPrelude as P hiding (Int) 

import Singleton

import Type.Feldspar.ADT
import Type.Feldspar.GADT ()

data Exp :: Typ -> * where 
  Exp :: Trm t -> Exp t
  
mapTrm :: (Trm t -> Trm t') -> Exp t -> Exp t'
mapTrm f (Exp x) = Exp (f x) 

(===) ::(Eq t', Trm t ~ t') =>
        Exp t -> Exp t -> Bool 
(Exp x) === (Exp y) = x == y

getTrm :: Exp t -> Trm t
getTrm (Exp x) = x

var :: t -> t
var = id

conI :: Integer -> Exp Int
conI = Exp 
     
conB :: Bool -> Exp Bol
conB = Exp

abs :: (Trm ta -> Trm tb) -> Exp (Arr ta tb)
abs = Exp 
        
app :: Exp (Arr ta tb) -> Exp ta -> Exp tb
app (Exp vf) (Exp va) = Exp (vf va)

addV :: Exp (Arr Int (Arr Int Int))
addV = Exp (+)

cnd :: Exp Bol -> Exp a -> Exp a -> Exp a
cnd (Exp vc) vt vf = if vc then vt else vf

whl :: Exp (Arr s  Bol) -> Exp (Arr s s) -> Exp s -> Exp s
whl (Exp fc) (Exp fb) =  Exp . head . dropWhile fc . iterate fb . getTrm 

tpl :: Exp tf -> Exp ts -> Exp (Tpl tf ts)
tpl (Exp vf) (Exp vs) = Exp (vf , vs)

fst :: Exp (Tpl a b) -> Exp a
fst (Exp v) = Exp (P.fst v)

snd :: Exp (Tpl a b) -> Exp b
snd (Exp v) = Exp (P.snd v)
 
ary :: Exp Int -> Exp (Arr Int a) -> Exp (Ary a)
ary (Exp vl) (Exp vf) =  Exp (listArray (0 , vl)  
                              [vf i | i <- [0 .. vl]])

len :: Exp (Ary a) -> Exp Int
len (Exp e)  = (Exp . (1 +) . uncurry (flip (-)) . bounds) e

ind :: Exp (Ary a) -> Exp Int -> Exp a
ind (Exp v) (Exp vi) = Exp (v ! vi)
 
lett :: Exp tl -> Exp (Arr tl tb) -> Exp tb
lett (Exp vl) (Exp vb) = Exp (vb vl)
  
