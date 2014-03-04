module Expression.Feldspar.GADTValue where

import Prelude as P
import Data.Array

var :: t -> t
var = id

conI :: Integer -> Integer
conI = id 
     
conB :: Bool -> Bool
conB = id 

abs :: (a -> b) -> (a -> b)
abs = id 
        
-- Application of two values
app :: (ta -> tb) -> ta -> tb
app = ($)

addV :: Integer -> Integer -> Integer
addV = (+)
 
cnd :: Bool -> a -> a -> a
cnd vc vt vf = if vc then vt else vf

whl :: (s -> Bool) -> (s -> s) -> s -> s
whl fc fb = head . dropWhile fc . iterate fb

fst :: (a , b) -> a
fst = P.fst 

snd :: (a , b) -> b
snd = P.snd 

tpl :: a -> b -> (a , b)
tpl = (,)
 
ary :: Integer -> (Integer -> a) -> (Array Integer a)
ary l vf = listArray (0 , l)  
           [vf i | i <- [0 .. l]]

len :: Array Integer a -> Integer
len = (1 +) . uncurry (flip (-)) . bounds

ind :: Array Integer a -> Integer -> a
ind = (!) 


lett :: tl -> (tl -> tb) -> tb
lett = flip ($)