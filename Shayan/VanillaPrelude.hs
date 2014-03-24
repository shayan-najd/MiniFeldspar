module VanillaPrelude where

import Data.Array

whl :: (s -> Bool) -> (s -> s) -> s -> s 
whl = undefined

fst :: (a , b) -> a
fst = undefined

snd :: (a , b) -> b
snd = undefined

ary :: Integer -> (Integer -> a) -> a
ary = undefined

len :: (Array Integer a) -> Integer
len = undefined

ind :: (Array Integer a) -> Integer -> a 
ind = undefined