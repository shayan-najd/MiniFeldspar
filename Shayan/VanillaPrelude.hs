module VanillaPrelude where

import MyPrelude as MP

cnd :: Bool -> s -> s -> s
cnd c t f = if c then t else f

whl :: (s -> Bool) -> (s -> s) -> s -> s
whl fc fb = head . dropWhile fc . iterate fb

tpl :: a -> b -> (a , b)
tpl = ((,))

fst :: (a , b) -> a
fst = MP.fst

snd :: (a , b) -> b
snd = MP.snd

ary :: Integer -> (Integer -> a) -> Array Integer a
ary l f = listArray (0 , l - 1) (fmap f [0 .. l - 1])

len :: (Array Integer a) -> Integer
len = (1 +) . uncurry (flip (-)) . bounds

ind :: (Array Integer a) -> Integer -> a
ind = (!)

cmx :: Float -> Float -> Complex Float
cmx = (:+)