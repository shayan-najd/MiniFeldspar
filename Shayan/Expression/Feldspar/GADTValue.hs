module Expression.Feldspar.GADTValue
    (Exp(..)
    ,conI,conB,conF,var,abs,app,cnd,whl,fst,snd,tpl,ary,len,ind,leT
    ,cmx,non,som,may
    ,getTrm,mapTrm) where

import MyPrelude hiding (Int,abs,fst,snd)
import qualified VanillaPrelude as VP

import Singleton

import Type.Feldspar.ADT
import Type.Feldspar.GADT ()

data Exp :: Typ -> * where
  Exp :: Trm t -> Exp t

mapTrm :: (Trm t -> Trm t') -> Exp t -> Exp t'
mapTrm f (Exp x) = Exp (f x)
{-
(===) :: (Eq t', Trm t ~ t') =>
         Exp t -> Exp t -> Bool
(Exp x) === (Exp y) = x == y
-}
getTrm :: Exp t -> Trm t
getTrm (Exp x) = x

var :: t -> t
var = id

conI :: Word32 -> Exp Int
conI = Exp

conB :: Bool -> Exp Bol
conB = Exp

conF :: Float -> Exp Flt
conF = Exp

abs :: (Trm ta -> Trm tb) -> Exp (Arr ta tb)
abs = Exp

app :: Exp (Arr ta tb) -> Exp ta -> Exp tb
app (Exp vf) (Exp va) = Exp (vf va)

cnd :: Exp Bol -> Exp a -> Exp a -> Exp a
cnd (Exp vc) (Exp vt) (Exp vf) = Exp (VP.cnd vc vt vf)

whl :: Exp (Arr s  Bol) -> Exp (Arr s s) -> Exp s -> Exp s
whl (Exp fc) (Exp fb) (Exp s) = Exp (VP.whl fc fb s)

tpl :: Exp tf -> Exp ts -> Exp (Tpl tf ts)
tpl (Exp vf) (Exp vs) = Exp (VP.tpl vf vs)

fst :: Exp (Tpl a b) -> Exp a
fst (Exp v) = Exp (VP.fst v)

snd :: Exp (Tpl a b) -> Exp b
snd (Exp v) = Exp (VP.snd v)

ary :: Exp Int -> Exp (Arr Int a) -> Exp (Ary a)
ary (Exp vl) (Exp vf) = Exp (VP.ary vl vf)

len :: Exp (Ary a) -> Exp Int
len (Exp e)  = Exp (VP.len e)

ind :: Exp (Ary a) -> Exp Int -> Exp a
ind (Exp v) (Exp vi) = Exp (VP.ind v vi)

leT :: Exp tl -> Exp (Arr tl tb) -> Exp tb
leT (Exp vl) (Exp vb) = Exp (vb vl)

cmx :: Exp Flt -> Exp Flt -> Exp Cmx
cmx (Exp fr) (Exp fi) = Exp (VP.cmx fr fi)

non :: Exp (May a)
non = Exp VP.non

som :: Exp a -> Exp (May a)
som (Exp e) = Exp (VP.som e)

may :: Exp (May a) -> Exp b -> Exp (Arr a b) -> Exp b
may (Exp em) (Exp en) (Exp es) = Exp (VP.may em en es)

